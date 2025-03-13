
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Author: Matthew Jay, matthew.jay@ucl.ac.uk
# Tested in R version 4.4.0
# Code list: srp_blackburn_v1
# # # # # # # # # # # # # # # # # # # # # # # # # # # 


# In this example, we do not attempt to create mutually exclusive (or
# overlapping) groups of SRPs. This mirrors Blackburn et al's original paper
# where SRPs were counted as one group. There are different ways of grouping
# these presentations. See, for example, the SRP list by Ní Chobhthaigh et al,
# also available through the ECHILD Phenotype Code List Repository, which 
# contains alternative and more up-to-date groupings.


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
srp_blackburn <- fread("codelists/srp_blackburn_v1.csv", stringsAsFactors = F)
rm(conn_str)

# Remove the dot from the code list file so we can link to HES
srp_blackburn[, code := gsub("\\.", "", code)]


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

# Create a binary flag that indicates whether an episode contains a relevant code.
diagnoses[, srp := F]

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

# We start with the codes only valid in the first diagnostic position.
diagnoses[substr(code, 1, 3) %in%
            srp_blackburn[nchar(code) == 3 & diag_position == "first"]$code &
            diag_n == 1,
          srp := T]

# Code R10 has a med_surg flag, meaning it should not be counted where one of
# the exclusion codes is present. These include diag and procedure codes.
med_surg <- fread("codelists/srp_blackburn_v1_medical.csv", stringsAsFactors = F)
med_surg[, code := gsub("\\.", "", code)]

diagnoses[, med_surg_diag := F]
diagnoses[substr(code, 1, 4) %in% med_surg[code_type == "icd10"]$code, med_surg_diag := T]
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
diagnoses[substr(code, 1, 3) == "R10" & (med_surg_diag == T | med_surg_opertn == T), srp := F]
diagnoses[, med_surg_diag := NULL]
diagnoses[, med_surg_opertn := NULL]
rm(operations, med_surg, opertn_cols)


# And now we can get the codes in any diagnostic position.
diagnoses[substr(code, 1, 3) %in%
            srp_blackburn[nchar(code) == 3 & diag_position == "any"]$code,
          srp := T]

# Now aggregate to episode level. We do this by taking the max() of our
# binary SRP variable by token_person_id and epistart. This aggregates
# to episode level beacause the max() of c(TRUE, FALSE) is TRUE.

# We will order the dataset first so it's easier to inspect should you wish to 
# do so.
diagnoses <- diagnoses[order(token_person_id, epistart)]
diagnoses[, srp_episode := as.logical(max(srp)), by = .(token_person_id, epistart)]

# Now we can drop all the episodes that are not SRP episodes (for the sake
# of saving memory).
diagnoses <- diagnoses[srp_episode == T]


# Deal with flags ---------------------------------------------------------

# We do not need to worry about the remaining two flags: selfharm_xz_code and
# pers_hist. The latter is only relevant when creating mutually exclusive groups.
# The former indicates that certain codes (T36-T50 and certain codes from the
# S block) are only valid if they are in the first diagnostic position AND
# there is an X or Z block self-harm code in any position. However, the latter
# are always counted as SRPs and so such episodes are always counted.


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

spine[, ever_srp_blackburn := token_person_id %in% diagnoses$token_person_id]

# Remove the temporary data from memory
rm(diagnoses, diag_cols, srp_blackburn, hes_2019)

# We now have some binary flags that indicate the presence of a code in this year.
table(spine$ever_srp_blackburn, useNA = "always")
