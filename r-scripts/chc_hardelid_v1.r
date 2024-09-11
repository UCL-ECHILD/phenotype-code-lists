
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Author: Matthew Jay, matthew.jay@ucl.ac.uk
# Code list: chc_hardelid_v1
# # # # # # # # # # # # # # # # # # # # # # # # # # # 


# global ------------------------------------------------------------------

# Set global settings, such as your working directory, load libraries and specify
# the ODBC connection string.

setwd("[file_path_omitted]/codelist_repo/")
assign(".lib.loc", c(.libPaths(), "[file_path_omitted]"), envir = environment(.libPaths))
library(data.table)
library(RODBC)

conn_str <- odbcDriverConnect("[connection_string_omitted]")


# load data and codelist --------------------------------------------------

hes_2019 <- data.table(sqlQuery(conn_str, "select top 100000 * from FILE0184861_HES_APC_2019"))
setnames(hes_2019, names(hes_2019), tolower(names(hes_2019)))
hardelid <- fread("codelists/chc_hardelid_v1.csv", stringsAsFactors = F)
rm(conn_str)


# preliminary cleaning ----------------------------------------------------

# Remove the dot from the code list file so we can link to HES
hardelid[, code := gsub("\\.", "", code)]

# There are some codes in HES with 5 or 6 characters, which primarily relate to
# the asterisk and dagger system (see the primer for details). As these are not
# relevant, we truncate all codes to the first 4 characters only. The below
# is a very fast way of looping through the relevant columns and applying the
# substr() funciton.
diag_cols <- names(hes_2019)[grepl("^diag", names(hes_2019))]
for (j in diag_cols) set(hes_2019, j = j, value = substr(hes_2019[, get(j)], 1, 4))
rm(j)


# convert to long format --------------------------------------------------

# It is significantly easier to work with the diagnostic data in long format.
# This is especially so when working across NPD and HES, or when adopting a 
# spine-based approach as we do in the "How To?" guides. But even here,
# working with just one year of HES data, it is quicker and easier to identify
# relevant episodes in long format and then specify flags in the wide format data.

diagnoses <- melt(hes_2019[, c("token_person_id",
                               "epikey",
                               "startage",
                               "admidate",
                               "disdate",
                               diag_cols),
                           with = F],
                  id.vars = c("token_person_id",
                              "epikey",
                              "startage",
                              "admidate",
                              "disdate"),
                  variable.name = "diag_n",
                  value.name = "code")

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
diagnoses <- merge(diagnoses,
                   hardelid[, c("code", "flag", "group", "subgroup")],
                   by.x = "diag_for_link",
                   by.y = "code",
                   all.x = T)


# deal with flags ---------------------------------------------------------

# We now have a long format data table that contains codes in the target code list.
# However, some may violate specific conditions. We therefore want to remove these
# from the data table as they are not valid. We start by creating a flag for rows
# to drop (FALSE for all to begin with).
diagnoses[, drop := F]

# And then we set it to TRUE where a flag exists and the condition is not met.
# For chc_hardelid_v1, these are flags where a child must be greater than 10
# years old (AGE10) or where the length of admission is 3 nights or longer (LOS3).

# Age episodes can be dropped using startage (remembering that startages of 
# children aged <1 year are coded 7001 to 7007)
diagnoses[flag == "age10" & (startage < 10 | startage >= 7001), drop := T]

# For LOS3, we need to define the admission length, not episode length.
# In real-world analyses, you will need to clean episode and admission dates
# Here, we just use admidate and disdate and we set missing disdate
# to the end of the financial year and otherwise take these variables at face value.
# This approach alone may not be sufficient in real-world settings.
diagnoses[, admidate := as.Date(admidate, format = "%Y-%m-%d")]
diagnoses[, disdate := as.Date(disdate, format = "%Y-%m-%d")]
diagnoses[disdate < as.Date("1900-01-01"), disdate := as.Date("2020-03-31")]

diagnoses[, admi_los_nights := as.integer(difftime(disdate, admidate, units = "days"))]
diagnoses[flag == "los3" & admi_los_nights < 3, drop := T]

# Now drop the rows where the conditions are violated, leaving us with a dataset
# of episodes containing codes that are validly in the target list.
diagnoses <- diagnoses[drop == F]
diagnoses[, drop := NULL]


# create flags in the original data ---------------------------------------

# We can now create flags in our original, wide format data (or in your cohort
# spine if you are using a spine-based approach). For the sake of demonstration,
# here we just create a flag for any code in the Hardelid and three of the
# groups.

hes_2019[, chc_hardelid_any := token_person_id %in% diagnoses$token_person_id]
hes_2019[, chc_hardelid_resp := token_person_id %in% diagnoses[group == "respiratory"]$token_person_id]
hes_2019[, chc_hardelid_neuro := token_person_id %in% diagnoses[group == "neurological"]$token_person_id]
hes_2019[, chc_hardelid_cancerblood := token_person_id %in% diagnoses[group == "cancer/blood"]$token_person_id]

# Remove the temporary data from memory
rm(diagnoses, diag_cols, hardelid)

# We now have some binary flags that indicate the presence of a code in this year.
table(hes_2019$chc_hardelid_any, useNA = "always")
table(hes_2019$chc_hardelid_resp, useNA = "always")
table(hes_2019$chc_hardelid_neuro, useNA = "always")
table(hes_2019$chc_hardelid_cancerblood, useNA = "always")

# You can easily expand this approach by, for example, subsetting by age or year of 
# activity if you need conditions within a certain window (e.g. in a number)
# of years prior to starting school.