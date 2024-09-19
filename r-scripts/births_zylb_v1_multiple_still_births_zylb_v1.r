
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Author: Matthew Jay, matthew.jay@ucl.ac.uk
# Code list: births_zylb_v1 and multiple_still_births_zylb_v1
# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Note that this script combines the code list for identifying birth episodes
# and then excluding those with multiple or still births as per Zylbersztejn et
# al's original paper

# global ------------------------------------------------------------------

# Set global settings, such as your working directory, load libraries and specify
# the ODBC connection string.

setwd("[path_omitted]")
assign(".lib.loc", c(.libPaths(), "[path_omitted]"), envir = environment(.libPaths))
library(data.table)
library(RODBC)

conn_str <- odbcDriverConnect("[connection_str_omitted]")


# load data and codelist --------------------------------------------------

hes_2019 <- data.table(sqlQuery(conn_str, "select top 100000 * from FILE0184861_HES_APC_2019"))
setnames(hes_2019, names(hes_2019), tolower(names(hes_2019)))
births <- fread("codelists/births_zylb_v1.csv", stringsAsFactors = F)
rm(conn_str)


# the plan ----------------------------------------------------------------

# This code list uses a combination of diagnostic and other information to 
# identify birth episodes. After subsetting to episodes with startage 7001 or 
# 7002 (startage_7001_7002, i.e. those aged less than 6 days),
# we will start with the diagnoses, which are the most complex simply
# because there are 20 diag columns, and then deal with the others.

# We will start by defining a flag that indicates birth episodes, setting it
# to TRUE each time we find a relevant code.
hes_2019[, birth_episode := F]

# startage ----------------------------------------------------------------

hes_2019 <- hes_2019[startage %in% 7001:7002]

# diag --------------------------------------------------------------------

# Just like in the other code lists, we will convert the diag fields into 
# long format, identify records with either of the two birth ICD-10 codes
# and then merge this information back into our wide-format data.

diag_cols <- names(hes_2019)[grepl("^diag", names(hes_2019))]

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

# Now identify brith diags
diagnoses[, birth_diag := F]
diagnoses[substr(code, 1, 3) %in% births[field == "diag"]$code, birth_diag := T]
diagnoses <- diagnoses[birth_diag == T]
diagnoses <- diagnoses[, c("token_person_id", "epikey", "birth_diag")]
diagnoses <- diagnoses[!duplicated(diagnoses)]

# And merge back
hes_2019 <- merge(hes_2019,
                  diagnoses,
                  by = c("token_person_id", "epikey"),
                  all.x = T)

# And now set our flag.
hes_2019[birth_diag == T, birth_episode := T]
hes_2019[, birth_diag := NULL]

# And remove our temporary data to save memory
rm(diagnoses)

# other fields ------------------------------------------------------------

# hrg - What are the columns called?
names(hes_2019)[grepl("hrg", names(hes_2019))]

# hrgnhs is available to 2008/9 and suscorehrg and sushrg from
# 2009/10. Because in this example we are only using data from 2019, we will
# comment out the line for hrgnhs, but remember to include it
# if working with older data.

# hes_2019[hrgnhs %in% births[field == "hrg"]$code, birth_episode := T]
hes_2019[suscorehrg %in% births[field == "hrg"]$code, birth_episode := T]
hes_2019[sushrg %in% births[field == "hrg"]$code, birth_episode := T]


# With the remaining variables, we just hard code the values we want as they
# are quite simple. You could refer to the birth data.table, as above, if you
# wish.

# epitype
hes_2019[epitype %in% c(3, 6), birth_episode := T]


# classpat
hes_2019[classpat == 5, birth_episode := T]


# admimeth
hes_2019[admimeth %in% c("82", "83", "2C"), birth_episode := T]


# neocare
hes_2019[neocare %in% 0:3, birth_episode := T]


# Having carried out each of these steps, we are left with a dataset, most of
# whose rows are identified as birth episodes. We can drop the remaining episodes.
table(hes_2019$birth_episode, useNA = "always")

hes_2019 <- hes_2019[birth_episode == T]



# exclude still and multiple births ---------------------------------------

# We will adopt a similar method to the above. In this example, we combine all
# three of stillbirths, terminations and multiples together, though you could
# easily separate them out if you want to be able to count each.

hes_2019[, still_termination_multiple := F]

still <- fread("codelists/multiple_still_births_zylb_v1.csv")

# Remove the dot from the code list file so we can link to HES
still[, code := gsub("\\.", "", code)]

# diagnostic fields
diagnoses <- melt(hes_2019[, c("token_person_id",
                               "epikey",
                               diag_cols),
                           with = F],
                  id.vars = c("token_person_id",
                              "epikey"),
                  variable.name = "diag_n",
                  value.name = "code")

diagnoses <- diagnoses[code != ""]

diagnoses[, stillbirth := F]
diagnoses[, termination := F]
diagnoses[, multiple := F]

diagnoses[substr(code, 1, 3) %in% still[field == "diag" & group == "stillbirth" & nchar(code) == 3]$code, stillbirth := T]
diagnoses[substr(code, 1, 4) %in% still[field == "diag" & group == "stillbirth" & nchar(code) == 4]$code, stillbirth := T]
diagnoses[substr(code, 1, 4) %in% still[field == "diag" & group == "termination"]$code, termination := T]
diagnoses[substr(code, 1, 4) %in% still[field == "diag" & group == "multiple_births"]$code, multiple := T]

hes_2019 <- merge(hes_2019,
                  diagnoses[, c("token_person_id", "epikey", "stillbirth", "termination", "multiple")],
                  by = c("token_person_id", "epikey"),
                  all.x = T)

hes_2019[stillbirth == T | termination == T | multiple == T, still_termination_multiple := T]
hes_2019[, stillbirth := NULL]
hes_2019[, termination := NULL]
hes_2019[, multiple := NULL]
rm(diagnoses)


# birth order
birordr <- melt(hes_2019[, c("token_person_id",
                             "epikey",
                             names(hes_2019)[grepl("birordr", names(hes_2019))]),
                         with = F],
                id.vars = c("token_person_id",
                            "epikey"),
                variable.name = "diag_n",
                value.name = "code")

birordr <- birordr[!is.na(code) & code != "X" & code != ""]
birordr <- birordr[as.integer(code) > 1]
setnames(birordr, "code", "birordr_code")
birordr <- birordr[!duplicated(birordr)]

hes_2019 <- merge(hes_2019,
                  birordr[, c("token_person_id", "epikey", "birordr_code")],
                  by = c("token_person_id", "epikey"),
                  all.x = T)

hes_2019[!is.na(birordr_code), still_termination_multiple := T]
hes_2019[, birordr_code := NULL]
rm(birordr)


# dismeth
hes_2019[dismeth == 5, still_termination_multiple := T]


# birstat
birstat <- melt(hes_2019[, c("token_person_id",
                             "epikey",
                             names(hes_2019)[grepl("birstat", names(hes_2019))]),
                         with = F],
                id.vars = c("token_person_id",
                            "epikey"),
                variable.name = "diag_n",
                value.name = "code")

birstat <- birstat[!is.na(code)]
birstat <- birstat[code %in% 2:4]
setnames(birstat, "code", "birstat_code")
birstat <- birstat[!duplicated(birstat)]

hes_2019 <- merge(hes_2019,
                  birstat[, c("token_person_id", "epikey", "birstat_code")],
                  by = c("token_person_id", "epikey"),
                  all.x = T)

hes_2019[!is.na(birstat_code), still_termination_multiple := T]
hes_2019[, birstat_code := NULL]
rm(birstat)


# We can now drop these rows if you wish to leave us with a dataset of singleton
# live births.
table(hes_2019$still_termination_multiple, useNA = "always")
hes_2019 <- hes_2019[still_termination_multiple == F]
