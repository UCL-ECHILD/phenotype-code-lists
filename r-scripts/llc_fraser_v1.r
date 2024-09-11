
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Author: Matthew Jay, matthew.jay@ucl.ac.uk
# Code list: llc_fraser_v1
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
fraser <- fread("codelists/llc_fraser_v1.csv", stringsAsFactors = F)
rm(conn_str)


# preliminary cleaning ----------------------------------------------------

# Remove the dot from the code list file so we can link to HES
fraser[, code := gsub("\\.", "", code)]

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
diagnoses[, llc_fraser := F]

# identify episodes with relevant codes -----------------------------------

# Here we look for both the 3 and 4 character codes and set the new binary
# indicator to TRUE where a relevant code is found.
# (If we had not converted to long format, we would have to apply this operation
# across several columns, which is difficult to code and may take a very significant
# amount of time, especially when using a for loop or the apply() functions.)

diagnoses[substr(code, 1, 3) %in% fraser[nchar(code) == 3]$code, llc_fraser := T]
diagnoses[substr(code, 1, 4) %in% fraser[nchar(code) == 4]$code, llc_fraser := T]

# Now we can drop all the rows that are not in the code list as well as the now-
# redundant indicator variable.
diagnoses <- diagnoses[llc_fraser == T]
diagnoses[, llc_fraser := NULL]

# deal with flags ---------------------------------------------------------

# There are no flags in the Fraser et al code list.


# create flags in the original data ---------------------------------------

# We can now create a flag in our original, wide format data (or in your cohort
# spine if you are using a spine-based approach).

hes_2019[, llc_fraser := token_person_id %in% diagnoses$token_person_id]

# Remove the temporary data from memory
rm(diagnoses, diag_cols, fraser)

# We now have a binary flag that indicates the presence of a code in this year.
table(hes_2019$llc_fraser, useNA = "always")

# You can easily expand this approach by, for example, subsetting by age or year of 
# activity if you need conditions within a certain window (e.g. in a number)
# of years prior to starting school.