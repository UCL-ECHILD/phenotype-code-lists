
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Author: Matthew Jay, matthew.jay@ucl.ac.uk
# Code list: chc_hardelid_v1
# Tested in R version 4.4.0
# # # # # # # # # # # # # # # # # # # # # # # # # # # 


# set-up ------------------------------------------------------------------

# Clear workspace
rm(list=ls())

# Set global settings, such as your working directory, load libraries and specify
# the ODBC connection string.

setwd("[omitted]")
assign(".lib.loc", c(.libPaths(), "[omitted]"), envir = environment(.libPaths))
library(data.table)
library(RODBC)

conn_str <- odbcDriverConnect("[omitted]")


# load data and codelist --------------------------------------------------

hes_2019 <- data.table(sqlQuery(conn_str, "select top 100000 * from FILE0184861_HES_APC_2019"))
setnames(hes_2019, names(hes_2019), tolower(names(hes_2019)))
hardelid <- fread("codelists/chc_hardelid_v1.csv", stringsAsFactors = F)
rm(conn_str)

# Remove the dot from the code list file so we can link to HES
hardelid[, code := gsub("\\.", "", code)]


# convert to long format --------------------------------------------------

# It is significantly easier to work with the diagnostic data in long format.
# This is especially so when working across NPD and HES, or when adopting a 
# spine-based approach as we do in the "How To?" guides. But even here,
# working with just one year of HES data, it is quicker and easier to identify
# relevant episodes in long format and then specify flags in the wide format data.

diag_cols <-
  names(hes_2019)[grepl("^diag", names(hes_2019))]

diagnoses <-
  melt(hes_2019[, c("token_person_id",
                    "epikey",
                    "startage",
                    "admidate",
                    "disdate",
                    "epiend",
                    diag_cols),
                with = F],
       id.vars = c("token_person_id",
                   "epikey",
                   "startage",
                   "admidate",
                   "disdate",
                   "epiend"),
       variable.name = "diag_n",
       value.name = "code")

# There are some codes in HES with 5 or 6 characters, some of which which relate
# to the asterisk and dagger system (see the primer for details). As these are not
# relevant, we truncate all codes to the first 4 characters only.
diagnoses[, code := substr(code, 1, 4)]

# We can drop empty rows (i.e. where no diagnosis was recorded in a given position)
# as these are now redundant.
diagnoses <- diagnoses[code != ""]


# create a binary flag ----------------------------------------------------

# Create a binary flag that indicates whether an episode contains a relevant code.
# The plans is:
# 1. This variable is FALSE for all episodes to begin with.
# 2. Where we find a code that is in the code list, we set the variable to TRUE.
# 3. We then deal with special flags, dropping rows from our temporary long format
# dataset where the special conditions are not met.
diagnoses[, chc_hardelid := F]

# We also need a variable that enables us to link the code description and groups.
# (This is because the code used in HES might be a 4 character code that is included
# because its 3 character parent code is in the code list. However, if we tried
# to link the code description and groups using the 4 character code, it would fail
# as the 4 character verision is recorded explicitly in the code list.)
diagnoses[, diag_for_link := code]


# identify episodes with relevant codes -----------------------------------

# Here we look for both the 3 and 4 character codes and set the new binary
# indicator to TRUE where a relevant code is found.
# (If we had not converted to long format, we would have to apply this operation
# across several columns, which is difficult to code and may take a very significant
# amount of time, especially when using a for loop or the apply() functions.)
diagnoses[substr(code, 1, 3) %in% hardelid[nchar(code) == 3]$code, chc_hardelid := T]
diagnoses[substr(code, 1, 3) %in% hardelid[nchar(code) == 3]$code, diag_for_link := substr(code, 1, 3)]

diagnoses[substr(code, 1, 4) %in% hardelid[nchar(code) == 4]$code, chc_hardelid := T]
diagnoses[substr(code, 1, 4) %in% hardelid[nchar(code) == 4]$code, diag_for_link := substr(code, 1, 4)]

# Now we can drop all the rows that are not in the code list as well as the now-
# redundant indicator variable.
diagnoses <- diagnoses[chc_hardelid == T]
diagnoses[, chc_hardelid := NULL]

# Now we left join the flags and sub-groups
diagnoses <-
  merge(
    diagnoses,
    hardelid[, c("code", "flag", "group", "subgroup")],
    by.x = "diag_for_link",
    by.y = "code",
    all.x = T
  )


# deal with flags ---------------------------------------------------------

# We now have a long format data table that contains codes in the target code list.
# However, some may violate specific conditions. We therefore want to remove these
# from the data table as they are not valid. We start by creating a flag for rows
# to drop (FALSE for all to begin with).
diagnoses[, drop := F]

# And then we set it to TRUE where a flag exists and the condition is not met.
# For chc_hardelid_v1, these are flags where a child must be greater than 10
# years old (AGE10) or where the length of admission is 3 nights or longer (LOS3).

# Age episodes can be dropped using startage (remembering that startages of 7001
# or higher are for children aged less than one year).
diagnoses[flag == "age10" & (startage < 10 | startage >= 7001), drop := T]

# For LOS3, we need to define the admission length, not episode length.
# In real-world analyses, you will need to clean episode and admission dates
# Here, we just use admidate and disdate. We also set missing disdates to epiend.
# Otherwise, we take these variables at face value.
# This approach alone may not be sufficient in real-world settings.
diagnoses[, admidate := as.Date(admidate, format = "%Y-%m-%d")]
diagnoses[, disdate := as.Date(disdate, format = "%Y-%m-%d")]
diagnoses[, epiend := as.Date(epiend, format = "%Y-%m-%d")]
diagnoses[disdate < as.Date("1900-01-01") | is.na(disdate), disdate := epiend]

diagnoses[, admi_los_nights := as.integer(difftime(disdate, admidate, units = "days"))]
diagnoses[flag == "los3" & admi_los_nights < 3, drop := T]

# Now drop the rows where the conditions are violated, leaving us with a dataset
# of episodes containing codes that are validly in the target list.
diagnoses <- diagnoses[drop == F]
diagnoses[, drop := NULL]


# Create spine and flag ---------------------------------------------------

# We will here create a data table that contains one row per patient and then
# create flags to indicate whether a patient ever had a relevant CHC code.
# In real world settings, you might be doing this using a spine of study
# participants created elsewhere and using information only over certain time
# periods. See the ECHILD How To guides for more information and examples.

spine <-
  data.table(
    token_person_id = unique(hes_2019$token_person_id)
  )

# We will just create flags for just some groups for the sake of example.
spine[, chc_hardelid_any := token_person_id %in% diagnoses$token_person_id]
spine[, chc_hardelid_resp := token_person_id %in% diagnoses[group == "respiratory"]$token_person_id]
spine[, chc_hardelid_neuro := token_person_id %in% diagnoses[group == "neurological"]$token_person_id]
spine[, chc_hardelid_cancerblood := token_person_id %in% diagnoses[group == "cancer/blood"]$token_person_id]

# Remove the temporary data from memory
rm(diagnoses, diag_cols, hardelid, hes_2019)

# We now have some binary flags that indicate the presence of a code in this year.
table(spine$chc_hardelid_any, useNA = "always")
table(spine$chc_hardelid_resp, useNA = "always")
table(spine$chc_hardelid_neuro, useNA = "always")
table(spine$chc_hardelid_cancerblood, useNA = "always")
