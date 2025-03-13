
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Author: Matthew Jay, matthew.jay@ucl.ac.uk
# Code list: anorectal_ford_v1
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

hes_2019 <- data.table(sqlQuery(conn_str, "select * from FILE0184861_HES_APC_2019"))
setnames(hes_2019, names(hes_2019), tolower(names(hes_2019)))
anorectal_ford <- fread("codelists/anorectal_ford_v1.csv", stringsAsFactors = F)
rm(conn_str)

# Remove the dot from the code list file so we can link to HES
anorectal_ford[, code := gsub("\\.", "", code)]


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
                    diag_cols),
                with = F],
       id.vars = c("token_person_id",
                   "epikey",
                   "startage"),
       variable.name = "diag_n",
       value.name = "code")

# There are some codes in HES with 5 or 6 characters, some of which which relate
# to the asterisk and dagger system (see the primer for details). As these are not
# relevant, we truncate all codes to the first 4 characters only.
diagnoses[, code := substr(code, 1, 4)]

# We can drop empty rows (i.e. where no diagnosis was recorded in a given position)
# as these are now redundant.
diagnoses <- diagnoses[code != ""]

# Now subset to infant-aged records only
diagnoses <- diagnoses[startage >= 7001]


# Now do the same for operations.

# We also need birthweight and gestational age to deal with the bwt_2500_ga_37 flag.
# In reality, this should be cleaned using mother-baby linkage where possible.
# We will take a simple approach here and just use birweit_1 and gestat_1

opertn_cols <-
  names(hes_2019)[grepl("opertn", names(hes_2019))]

operations <-
  melt(hes_2019[, c("token_person_id",
                    "epikey",
                    "startage",
                    opertn_cols),
                with = F],
       id.vars = c("token_person_id",
                   "epikey",
                   "startage"),
       variable.name = "diag_n",
       value.name = "code")

operations <- operations[!(code %in% c("", "-"))]

operations <- operations[startage >= 7001]


# create a binary flag ----------------------------------------------------

# Create a binary flag that indicates whether an episode contains a relevant code.
# The plans is:
# 1. This variable is FALSE for all episodes to begin with.
# 2. Where we find a code that is in the code list, we set the variable to TRUE.
# 3. We then deal with special flags, dropping rows from our temporary long format
# dataset where the special conditions are not met.
diagnoses[, anorectal := F]
operations[, anorectal := F]


# identify episodes with relevant codes -----------------------------------

# Here we look for both the 4 character codes and set the new binary
# indicator to TRUE where a relevant code is found.
# (If we had not converted to long format, we would have to apply this operation
# across several columns, which is difficult to code and may take a very significant
# amount of time, especially when using a for loop or the apply() functions.)
diagnoses[substr(code, 1, 4) %in% anorectal_ford[nchar(code) == 4 & code_type == "icd10"]$code, anorectal := T]
operations[substr(code, 1, 4) %in% anorectal_ford[nchar(code) == 4 & code_type == "opcs4"]$code, anorectal := T]

# Now we can drop all the rows that are not in the code list as well as the now-
# redundant indicator variable.
diagnoses <- diagnoses[anorectal == T]
diagnoses[, anorectal := NULL]

operations <- operations[anorectal == T]
operations[, anorectal := NULL]


# deal with flags ---------------------------------------------------------

# We have already dealt with flag1 (infant-age records only).
# We now need to deal with the few operation codes with flag2, which tells us to
# ignore codes where a differential diagnosis is present in the first year of
# life.

# We start by creaeting a binary variable that we will use to drop invalid
# codes, setting this to FALSE to start with.
diagnoses[, drop := F]
operations[, drop := F]

# We then identify the differential diagnosis in much the same way as above.
anorectal_diff <- fread("codelists/anorectal_ford_v1_differential_diag.csv",
                        stringsAsFactors = F)
anorectal_diff[, code := gsub("\\.", "", code)]

diff_diag <-
  melt(hes_2019[, c("token_person_id",
                    "epikey",
                    "startage",
                    diag_cols),
                with = F],
       id.vars = c("token_person_id",
                   "epikey",
                   "startage"),
       variable.name = "diag_n",
       value.name = "code")

diff_diag[, code := substr(code, 1, 4)]
diff_diag <- diff_diag[code != ""]
diff_diag <- diff_diag[startage >= 7001]

diff_diag[, differential := F]
diff_diag[substr(code, 1, 3) %in% anorectal_diff[nchar(code) == 3]$code, differential := T]
diff_diag[substr(code, 1, 4) %in% anorectal_diff[nchar(code) == 4]$code, differential := T]

diff_diag <- diff_diag[differential == T]
diff_diag[, differential := NULL]

# And then we set the indicator variable to TRUE where a child has an infant-age
# differential diagnosis.
diagnoses[token_person_id %in% diff_diag$token_person_id, drop := T]
operations[token_person_id %in% diff_diag$token_person_id, drop := T]

# Now drop the rows where the conditions are violated, leaving us with a dataset
# of episodes containing codes that are validly in the target list.
diagnoses <- diagnoses[drop == F]
diagnoses[, drop := NULL]

operations <- operations[drop == F]
operations[, drop := NULL]


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

spine[, anorectal := token_person_id %in% diagnoses$token_person_id | 
           token_person_id %in% operations$token_person_id]

# Remove the temporary data from memory
rm(diagnoses, operations, diag_cols, opertn_cols, anorectal, diff_diag,
   anorectal_ford, hes_2019)

# We now have some binary flags that indicate the presence of a code in this year.
table(spine$anorectal, useNA = "always")

