
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Author: Matthew Jay, matthew.jay@ucl.ac.uk
# Tested in R version 4.4.0
# Code list: srp_nichobhthaigh_v2
# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# In this script, we adopt a very basic approach to categorising the codes into
# potentially psychosomatic, interanlising, externalising, thought disorders
# and self-harm. The potentially pyschosomatic, internalising and thought
# disorder codes are relatively straight forward. Some of the externalising
# codes, however, are only valid if an X or Z block self-harm code is also
# present in a secondary diagnostic position. There are also some codes in the
# self-harm group where the same is true.

# In this script, where a self-harm code is recorded any any diagnostic position,
# we count the episode as a self-harm episode. As such, the above externalising
# and self-harm codes will always be superceded by the self-harm codes and we
# essentially ignore them. You may wish to adopt a different approach, especially
# if you are interested in multimorbidity.


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
srp_nichobhthaigh <- fread("codelists/srp_nichobhthaigh_v2.csv", stringsAsFactors = F)
rm(conn_str)

# Remove the dot from the code list file so we can link to HES
srp_nichobhthaigh[, code := gsub("\\.", "", code)]


# convert to long format --------------------------------------------------

# It is significantly easier to work with the diagnostic data in long format.
# This is especially so when working across NPD and HES, or when adopting a 
# spine-based approach as we do in the "How To?" guides. But even here,
# working with just one year of HES data, it is quicker and easier to identify
# relevant episodes in long format and then specify flags in the wide format data.

# We also need only emergency admissions, and so we use the emergency admissions
# code list to identify and retain only these in our long-format data.

diag_cols <-
  names(hes_2019)[grepl("^diag", names(hes_2019))]

diagnoses <-
  melt(hes_2019[, c("token_person_id",
                    "epikey",
                    "epistart",
                    "admimeth",
                    diag_cols),
                with = F],
       id.vars = c("token_person_id",
                   "epikey",
                   "epistart",
                   "admimeth"),
       variable.name = "diag_n",
       value.name = "code")

# There are some codes in HES with 5 or 6 characters, some of which which relate
# to the asterisk and dagger system (see the primer for details). As these are not
# relevant, we truncate all codes to the first 4 characters only.
diagnoses[, code := substr(code, 1, 4)]

# We can drop empty rows (i.e. where no diagnosis was recorded in a given position)
# as these are now redundant.
diagnoses <- diagnoses[code != ""]

# Retain only emergency admissions
em_adm <- fread("codelists/emergency_admissions_v1.csv")
diagnoses <- diagnoses[admimeth %in% em_adm$code]
rm(em_adm)


# create a binary flag ----------------------------------------------------

# Create binary flags that indicate whether an episode contains a relevant code.
# We will create different flags for different categories for SRP.
diagnoses[, psych := F]         # Potentially psychosomatic
diagnoses[, internalising := F] # Internalising
diagnoses[, externalising := F] # Externalising
diagnoses[, thought_dis := F]   # Thought disorders
diagnoses[, selfharm := F]      # Self-harm

# Most codes are only counted if they are in the first diagnostic position. We
# therefore also need to create a variable that contains the diag number. We
# will use the existing diag_n variable and remove everything before and
# including the underscore.
diagnoses[, diag_n := as.integer(gsub("diag_", "", diag_n))]


# identify episodes with relevant codes -----------------------------------

# Here we look for both codes and set the new binary
# indicator to TRUE where a relevant code is found.
# (If we had not converted to long format, we would have to apply this operation
# across several columns, which is difficult to code and may take a very significant
# amount of time, especially when using a for loop or the apply() functions.)


# Potentially psychosomatic
diagnoses[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "potentially_psych"]$code &
            diag_n == 1, # All first diagnostic position only
          psych := T]

diagnoses[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "potentially_psych"]$code &
            diag_n == 1, # All first diagnostic position only
          psych := T]

# These codes have a med_surg flag, meaning it should not be counted where one of
# the exclusion codes is present. These include diag and procedure codes.
med_surg <- fread("codelists/srp_nichobhthaigh_v2_medical.csv", stringsAsFactors = F)
med_surg[, code := gsub("\\.", "", code)]

diagnoses[, med_surg_diag := F]
diagnoses[substr(code, 1, 3) %in% med_surg[code_type == "icd10" & nchar(code) == 3]$code, med_surg_diag := T]
diagnoses[substr(code, 1, 4) %in% med_surg[code_type == "icd10" & nchar(code) == 4]$code, med_surg_diag := T]
diagnoses[, med_surg_diag := max(med_surg_diag), by = .(token_person_id, epistart)]

opertn_cols <-
  names(hes_2019)[grepl("opertn", names(hes_2019))]

operations <-
  melt(hes_2019[, c("token_person_id",
                    "epistart",
                    opertn_cols),
                with = F],
       id.vars = c("token_person_id",
                   "epistart"),
       variable.name = "diag_n",
       value.name = "code")

operations <- operations[!(code %in% c("", "-"))]

operations[, med_surg_opertn := F]
operations[substr(code, 1, 4) %in% med_surg[code_type == "opcs4"]$code, med_surg_opertn := T]
operations[, med_surg_opertn := max(med_surg_opertn), by = .(token_person_id, epistart)]
operations <- operations[med_surg_opertn == T]
operations <- operations[, c("token_person_id", "epistart", "med_surg_opertn")]
operations <- operations[!duplicated(operations)]

diagnoses <-
  merge(
    diagnoses,
    operations,
    by = c("token_person_id", "epistart"),
    all.x = T
  )

# Now set to FALSE where the code is R10 and the episode is medical or surgical.
diagnoses[psych == T & (med_surg_diag == T | med_surg_opertn == T), psych := F]
diagnoses[, med_surg_diag := NULL]
diagnoses[, med_surg_opertn := NULL]
rm(operations, med_surg, opertn_cols)


# Internalising
diagnoses[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "internalising"]$code &
            diag_n == 1, # All first diagnostic position only
          internalising := T]

diagnoses[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "internalising"]$code &
            diag_n == 1, # All first diagnostic position only
          internalising := T]


# Externalising
# There are some externalising codes that are only counted if an X or Y block
# self-harm code is present in a secondary position. However, these will always
# be counted as self-harm codes and so we ignore them here.
diagnoses[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "externalising" & flag2 != "selfharm_xz_codes"]$code &
            diag_n == 1, # All first diagnostic position only
          externalising := T]

diagnoses[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "externalising" & flag2 != "selfharm_xz_codes"]$code &
            diag_n == 1, # All first diagnostic position only
          externalising := T]



# Thought disorder
diagnoses[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "thought_disorder"]$code &
            diag_n == 1, # All first diagnostic position only
          thought_dis := T]

diagnoses[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "thought_disorder"]$code &
            diag_n == 1, # All first diagnostic position only
          thought_dis := T]


# Self-harm
# There are some self-harm codes that are only counted if an X or Y block
# self-harm code is present in a secondary position. However, these will always
# be counted as self-harm codes and so we ignore them here.
diagnoses[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "selfharm" & flag2 != "selfharm_xz_codes"]$code,
          selfharm := T]

diagnoses[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "selfharm" & flag2 != "selfharm_xz_codes"]$code,
          selfharm := T]


# Now aggregate to episode level. We do this by taking the max() of our
# binary SRP variable by token_person_id and epistart. This aggregates
# to episode level beacause the max() of c(TRUE, FALSE) is TRUE.

# We will order the dataset first so it's easier to inspect should you wish to 
# do so.
diagnoses <- diagnoses[order(token_person_id, epistart)]
diagnoses[, psych_episode := as.logical(max(psych)), by = .(token_person_id, epistart)]
diagnoses[, internalising_episode := as.logical(max(internalising)), by = .(token_person_id, epistart)]
diagnoses[, externalising_episode := as.logical(max(externalising)), by = .(token_person_id, epistart)]
diagnoses[, thought_dis_episode := as.logical(max(thought_dis)), by = .(token_person_id, epistart)]
diagnoses[, selfharm_episode := as.logical(max(selfharm)), by = .(token_person_id, epistart)]

# Create variable to identify any SRP
diagnoses[, any_srp := psych_episode | internalising_episode | externalising_episode |
            thought_dis_episode | selfharm_episode]

# Now we can drop all the episodes that are not SRP episodes (for the sake
# of saving memory).
diagnoses <- diagnoses[any_srp == T]


# Deal with flags ---------------------------------------------------------

# We do not need to worry about the remaining selfharm_xz_code flag.
# This indicates that certain codes are only valid if they are in the first
# diagnostic position AND there is an X or Y block self-harm code in any
# position. However, the latter are always counted as SRPs and so such episodes
# are always counted.


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

spine[, ever_any_srp := token_person_id %in% diagnoses$token_person_id]
spine[, ever_psych := token_person_id %in% diagnoses[psych_episode == T]$token_person_id]
spine[, ever_internalising := token_person_id %in% diagnoses[internalising_episode == T]$token_person_id]
spine[, ever_externalising := token_person_id %in% diagnoses[externalising_episode == T]$token_person_id]
spine[, ever_thought_dis := token_person_id %in% diagnoses[thought_dis_episode == T]$token_person_id]
spine[, ever_selfharm := token_person_id %in% diagnoses[selfharm_episode == T]$token_person_id]

# Remove the temporary data from memory
rm(diagnoses, diag_cols, srp_nichobhthaigh, hes_2019)

# We now have some binary flags that indicate the presence of a code in this year.
table(spine$ever_any_srp, useNA = "always")
table(spine$ever_psych, useNA = "always")
table(spine$ever_internalising, useNA = "always")
table(spine$ever_externalising, useNA = "always")
table(spine$ever_thought_dis, useNA = "always")
table(spine$ever_selfharm, useNA = "always")

