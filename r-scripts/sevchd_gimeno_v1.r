
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Author: Matthew Jay, matthew.jay@ucl.ac.uk
# Code list: sevchd_gimeno_v1
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

hes_2019 <- data.table(sqlQuery(conn_str, "select * from FILE0184861_HES_APC_2019"))
setnames(hes_2019, names(hes_2019), tolower(names(hes_2019)))
gimeno <- fread("codelists/sevchd_gimeno_v1.csv", stringsAsFactors = F)
rm(conn_str)


# preliminary cleaning ----------------------------------------------------

# Remove the dot from the code list file so we can link to HES
gimeno[, code := gsub("\\.", "", code)]

# There are some codes in HES with 5 or 6 characters, which primarily relate to
# the asterisk and dagger system (see the primer for details). As these are not
# relevant, we truncate all codes to the first 4 characters only. The below
# is a very fast way of looping through the relevant columns and applying the
# substr() function.
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
                               diag_cols),
                           with = F],
                  id.vars = c("token_person_id",
                              "epikey"),
                  variable.name = "diag_n",
                  value.name = "code")

# We can drop empty rows (i.e. where no diagnosis was recorded in a given position)
# as these are now redundant.
diagnoses <- diagnoses[code != ""]


# Now do the same for operations. However, all the OPCS-4 codes are only valid
# where the patient is aged less than 5. We therefore need startage in order to
# drop records aged 5+.

# We also need birthweight and gestational age to deal with the bwt_2500_ga_37 flag.
# In reality, this should be cleaned using mother-baby linkage where possible.
# We will take a simple approach here and just use birweit_1 and gestat_1

opertn_cols <- names(hes_2019)[grepl("opertn", names(hes_2019))]

operations <- melt(hes_2019[, c("token_person_id",
                                "epikey",
                                "startage",
                                "birweit_1",
                                "gestat_1",
                                opertn_cols),
                            with = F],
                   id.vars = c("token_person_id",
                               "epikey",
                               "startage",
                               "birweit_1",
                               "gestat_1"),
                   variable.name = "diag_n",
                   value.name = "code")

operations <- operations[!(code %in% c("", "-"))]

# Now subset to age using startage, remembering that ages of less than 1 year
# are coded 7001 to 7007.
operations <- operations[startage < 5 | startage >= 7001]

# create a binary flag ----------------------------------------------------

# Create a binary flag that indicates whether an episode contains a relevant code.
# The plans is:
# 1. This variable is FALSE for all episodes to begin with.
# 2. Where we find a code that is in the code list, we set the variable to TRUE.
# 3. We then deal with special flags, dropping rows from our temporary long format
# dataset where the special conditions are not met.
diagnoses[, sevchd := F]
operations[, sevchd := F]

# We also need a variable that enables us to link the flags in the operation codes.
# (This is because the code used in HES might be a 4 character code that is included
# because its 3 character parent code is in the code list. However, if we tried
# to link the code description and groups using the 4 character code, it would fail
# as the 4 character verision is recorded explicitly in the code list.)
operations[, diag_for_link := code]


# identify episodes with relevant codes -----------------------------------

# Here we look for both the 3 and 4 character codes and set the new binary
# indicator to TRUE where a relevant code is found.
# (If we had not converted to long format, we would have to apply this operation
# across several columns, which is difficult to code and may take a very significant
# amount of time, especially when using a for loop or the apply() functions.)
diagnoses[substr(code, 1, 3) %in% gimeno[nchar(code) == 3 & code_type == "icd10"]$code, sevchd := T]
diagnoses[substr(code, 1, 4) %in% gimeno[nchar(code) == 4 & code_type == "icd10"]$code, sevchd := T]

operations[substr(code, 1, 3) %in% gimeno[nchar(code) == 3 & code_type == "opcs4"]$code, sevchd := T]
operations[substr(code, 1, 3) %in% gimeno[nchar(code) == 3 & code_type == "opcs4"]$code, op_for_link := substr(code, 1, 3)]
operations[substr(code, 1, 4) %in% gimeno[nchar(code) == 4 & code_type == "opcs4"]$code, sevchd := T]
operations[substr(code, 1, 4) %in% gimeno[nchar(code) == 4 & code_type == "opcs4"]$code, op_for_link := substr(code, 1, 4)]

# Now we can drop all the rows that are not in the code list as well as the now-
# redundant indicator variable.
diagnoses <- diagnoses[sevchd == T]
diagnoses[, sevchd := NULL]

operations <- operations[sevchd == T]
operations[, sevchd := NULL]

# Now we left join the flag into operations (there are no flags for diagnoses) 
operations <- merge(operations,
                    gimeno[, c("code", "flag2")],
                    by.x = "op_for_link",
                    by.y = "code",
                    all.x = T)


# deal with flags ---------------------------------------------------------

# We have already dealt with flag1 (operation codes only counted where age is
# less than 5). We now need to deal with the few operation codes with flag2 ==
# bwt_2500_ga_37 (birthweight > 2500 and gestational age >= 37).
# We start by creating a variable for rows to drop (FALSE for all to begin with).
operations[, drop := F]

# And then we set it to TRUE where a flag exists and the condition is not met.
# As noted above, we have adopted a very crude approach here. Ideally, you would
# be working with cleaned gestational age and birthweight data.
operations[flag2 == "bwt_2500_ga_37" & (birweit_1 <= 2500 | gestat_1 < 37), drop := T]

# Now drop the rows where the conditions are violated, leaving us with a dataset
# of episodes containing codes that are validly in the target list.
operations <- operations[drop == F]
operations[, drop := NULL]


# create flags in the original data ---------------------------------------

# We can now create flags in our original, wide format data (or in your cohort
# spine if you are using a spine-based approach).
hes_2019[, sevchd := token_person_id %in% diagnoses$token_person_id | 
           token_person_id %in% operations$token_person_id]

# Remove the temporary data from memory
rm(diagnoses, operations, diag_cols, opertn_cols, gimeno)

# We now have some binary flags that indicate the presence of a code in this year.
table(hes_2019$sevchd, useNA = "always")

# You can easily expand this approach by, for example, subsetting by age or year of 
# activity if you need conditions within a certain window (e.g. in a number)
# of years prior to starting school.