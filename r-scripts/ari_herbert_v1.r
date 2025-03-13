
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Author: Matthew Jay, matthew.jay@ucl.ac.uk
# Code list: ari_herbert_v1
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
herbert <- fread("codelists/ari_herbert_v1.csv", stringsAsFactors = F)
rm(conn_str)

# Remove the dot from the code list file so we can link to HES
herbert[, code := gsub("\\.", "", code)]

# smoking code ------------------------------------------------------------

# As per the documentation, you may want to consider whether to include code F17.1
# (tobacco - harmful use). If you want to exclude F17.1 but not the other F17
# subcodes, you will need to run the following.

new_f17 <- data.table(
  code = c("F17.0", paste0("F17.", 2:9)),
  dataset = "hes_apc",
  field = "diag",
  code_type = "icd10",
  description = "smoking_codes",
  group = "drug_alc",
  subgroup = "illict_drugs",
  flag1 = "emergency_admission",
  flag2 = ""
)

herbert <- rbind(herbert, new_f17)
herbert <- herbert[code != "F17"]
rm(new_f17)


# convert to long format --------------------------------------------------

# It is significantly easier to work with the diagnostic data in long format.
# This is especially so when working across NPD and HES, or when adopting a 
# spine-based approach as we do in the "How To?" guides. But even here,
# working with just one year of HES data, it is quicker and easier to identify
# relevant episodes in long format and then specify flags in the wide format data.

# Note that here we need admidate and epistart, as an admission is only
# counted as an injury admission if the injury codes are recorded in the first
# episode of each admission.

# In real world applications, we would, following our usual methodology, and that
# employed by Herbert et al, clean admission and episode dates and join admissions
# together where a 2nd admission begins within a day of a previous (i.e. treat
# them as the same admission). In this example, we omit this step for the sake
# of demonstrating use of the code list.

# We also need only emergency admissions, and so we use the emergency admissions
# code list to identify and retain only these in our long-format data.

diag_cols <-
  names(hes_2019)[grepl("^diag", names(hes_2019))]

diagnoses <-
  melt(hes_2019[, c("token_person_id",
                    "epikey",
                    "startage",
                    "admidate",
                    "epistart",
                    "admimeth",
                    diag_cols),
                with = F],
       id.vars = c("token_person_id",
                   "epikey",
                   "startage",
                   "admidate",
                   "epistart",
                   "admimeth"),
       variable.name = "diag_n",
       value.name = "code")

diagnoses <-
  diagnoses[order(token_person_id,
                  admidate,
                  epistart)]

# There are some codes in HES with 5 or 6 characters, some of which which relate
# to the asterisk and dagger system (see the primer for details). As these are not
# relevant, we truncate all codes to the first 4 characters only.
diagnoses[, code := substr(code, 1, 4)]

# We can drop empty rows (i.e. where no diagnosis was recorded in a given position)
# as these are now redundant.
diagnoses <- diagnoses[code != ""]

# Identify whether an episode is the first in admission or not. Here, we take
# a simple approach and say that an episode is the first if admidate and epistart
# are equal, though, again, you will need to ensure dates are cleaned in your
# applications.
diagnoses[, first_episode := admidate == epistart]

# Retain only emergency admissions
em_adm <- fread("codelists/emergency_admissions_v1.csv")
diagnoses <- diagnoses[admimeth %in% em_adm$code]
rm(em_adm)


# create a binary flag ----------------------------------------------------

# Create binary flags that indicate whether an episode contains a relevant code.
# The plans is:
# 1. These variables are FALSE for all episodes to begin with.
# 2. Where we find a code that is in the code list, we set the variable to TRUE.
# 3. We identify separately: (1) injuries; (2) adversity-related episodes; (3) accidents.
# 4. We then aggregate up to admission level and identify accident-related injuries
# and adversity-related injuries.

diagnoses[, injury_episode := F]
diagnoses[, adversity_episode := F]
diagnoses[, accident_episode := F]


# identify episodes with relevant codes -----------------------------------

# Here we look for both the 3 and 4 character codes and set the new binary
# indicator to TRUE where a relevant code is found.
# (If we had not converted to long format, we would have to apply this operation
# across several columns, which is difficult to code and may take a very significant
# amount of time, especially when using a for loop or the apply() functions.)

# First, we identify injury episodes, all of which are three character codes.
diagnoses[substr(code, 1, 3) %in% herbert[group == "injuries"]$code, injury_episode := T]

# And set these to FALSE where it is not the first episode
diagnoses[injury_episode == T & first_episode == F, injury_episode := F]

# Second, identify adversity episodes
diagnoses[substr(code, 1, 3) %in% herbert[!(group %in% c("accidents", "injuries")) & nchar(code) == 3]$code, adversity_episode := T]
diagnoses[substr(code, 1, 4) %in% herbert[!(group %in% c("accidents", "injuries")) & nchar(code) == 4]$code, adversity_episode := T]

# Third, identify accident episodes (all three character codes)
diagnoses[substr(code, 1, 3) %in% herbert[group == "accidents"]$code, accident_episode := T]

# Now we can drop all the rows that are not in the code list
diagnoses <- diagnoses[injury_episode | adversity_episode | accident_episode]

# Aggregate to admission-level
# Here, we just take the max() of each of the above flags by token_person_id
# and admidate. In other words, where an admission has any episode that has 
# injury == TRUE (TRUE being the max() of c(TRUE, FALSE)), then
# our new injury_admission variable will be set to TRUE for all rows
# belonging to the same admission of the same child. Otherwise,
# if all episodes in an admission are FALSE for injury_admission, then the max()
# of these is still FALSE.

diagnoses[, injury_admission := as.logical(max(injury_episode)), by = .(token_person_id, admidate)]
diagnoses[, adversity_admission := as.logical(max(adversity_episode)), by = .(token_person_id, admidate)]
diagnoses[, accident_admission := as.logical(max(accident_episode)), by = .(token_person_id, admidate)]

# And now identify adversity-related injuries and accident-related injuries
diagnoses[, adversity_injury_admission :=
            injury_admission &
            adversity_admission]

diagnoses[, accident_injury_admission :=
            injury_admission &
            !adversity_admission &
            accident_admission]

# Drop rows that do not contain any of these (for the sake of memory)
diagnoses <- diagnoses[adversity_injury_admission == T |
                         accident_injury_admission == T |
                         adversity_admission == T]


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

spine[, adversity_injury_admission := token_person_id %in%
           diagnoses[adversity_injury_admission == T]$token_person_id]

spine[, accident_injury_admission := token_person_id %in%
           diagnoses[accident_injury_admission == T]$token_person_id]

spine[, adversity_admission := token_person_id %in%
           diagnoses[adversity_admission == T]$token_person_id]

# Remove the temporary data from memory
rm(diagnoses, diag_cols, herbert, hes_2019)

# We now have some binary flags that indicate the presence of a code in this year.
table(spine$adversity_injury_admission, useNA = "always")
table(spine$accident_injury_admission, useNA = "always")
table(spine$adversity_admission, useNA = "always")