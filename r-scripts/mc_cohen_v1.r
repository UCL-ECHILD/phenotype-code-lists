
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Author: Matthew Jay, matthew.jay@ucl.ac.uk
# Code list: mc_cohen_v1
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
cohen <- fread("codelists/mc_cohen_v1.csv", stringsAsFactors = F)
rm(conn_str)


# preliminary cleaning ----------------------------------------------------

# Remove the dot from the code list file so we can link to HES
cohen[, code := gsub("\\.", "", code)]

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
diagnoses[, mc_cohen := F]

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
diagnoses[substr(code, 1, 3) %in% cohen[nchar(code) == 3]$code, mc_cohen := T]
diagnoses[substr(code, 1, 3) %in% cohen[nchar(code) == 3]$code, diag_for_link := substr(code, 1, 3)]

diagnoses[substr(code, 1, 4) %in% cohen[nchar(code) == 4]$code, mc_cohen := T]
diagnoses[substr(code, 1, 4) %in% cohen[nchar(code) == 4]$code, diag_for_link := substr(code, 1, 4)]

# Now we can drop all the rows that are not in the code list as well as the now-
# redundant indicator variable.
diagnoses <- diagnoses[mc_cohen == T]
diagnoses[, mc_cohen := NULL]

# Now we left join the flags and sub-groups
diagnoses <- merge(diagnoses,
                   cohen[, c("code", "flag", "group", "subgroup")],
                   by.x = "diag_for_link",
                   by.y = "code",
                   all.x = T)


# deal with flags ---------------------------------------------------------

# In the case of the Cohen et al code list, the only flag is one to indicate
# that some ICD-10-CA codes have been truncated. We will ignore these here.


# create flags in the original data ---------------------------------------

# We can now create flags in our original, wide format data (or in your cohort
# spine if you are using a spine-based approach). For the sake of demonstration,
# here we just create a flag for any code in the Cohen et al and the three main groups.

hes_2019[, mc_cohen_any := token_person_id %in% diagnoses$token_person_id]
hes_2019[, mc_cohen_ccc := token_person_id %in% diagnoses[group == "ccc"]$token_person_id]
hes_2019[, mc_cohen_neurol_imp := token_person_id %in% diagnoses[group == "neurol_imp"]$token_person_id]
hes_2019[, mc_cohen_tech_assistance := token_person_id %in% diagnoses[group == "technological_assistance"]$token_person_id]

# Remove the temporary data from memory
rm(diagnoses, diag_cols, cohen)

# We now have some binary flags that indicate the presence of a code in this year.
table(hes_2019$mc_cohen_any, useNA = "always")
table(hes_2019$mc_cohen_ccc, useNA = "always")
table(hes_2019$mc_cohen_neurol_imp, useNA = "always")
table(hes_2019$mc_cohen_tech_assistance, useNA = "always")

# You can easily expand this approach by, for example, subsetting by age or year of 
# activity if you need conditions within a certain window (e.g. in a number)
# of years prior to starting school.