
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Author: Matthew Jay, matthew.jay@ucl.ac.uk
# Checked by: Kate Lewis
# Tested in R version 4.4.0
# Code list: asthma_lut_v1
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
lut <- fread("codelists/asthma_lut_v1.csv", stringsAsFactors = F)
rm(conn_str)


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
                    "epistart",
                    diag_cols),
                with = F],
       id.vars = c("token_person_id",
                   "epikey",
                   "epistart"),
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
# 3. We then aggregate to episode level so we can later check whether one of
# the exclusion codes is present on the same episode.

diagnoses[, asthma := F]

# identify episodes with relevant codes -----------------------------------

# Here we look for both codes and set the new binary
# indicator to TRUE where a relevant code is found.
# (If we had not converted to long format, we would have to apply this operation
# across several columns, which is difficult to code and may take a very significant
# amount of time, especially when using a for loop or the apply() functions.)

diagnoses[substr(code, 1, 3) %in% lut$code, asthma := T]

# Now aggregate to episode level. We do this by taking the max() of our
# binary asthma variable by token_person_id and epistart. This aggregates
# to episode level beacause the max() of c(TRUE, FALSE) is TRUE.

# We will order the dataset first so it's easier to inspect should you wish to 
# do so.
diagnoses <- diagnoses[order(token_person_id, epistart)]
diagnoses[, asthma_episode := as.logical(max(asthma)), by = .(token_person_id, epistart)]

# Now we can drop all the episodes that are not asthma episodes (for the sake
# of saving memory).
diagnoses <- diagnoses[asthma_episode == T]


# deal with flags ---------------------------------------------------------

# Now we need to repeate the above process but for the exclusion codes.
# Where an asthma episode has an exclusion code, we will drop it as the asthma
# codes should not be counted.

lut_exclusion <- fread("codelists/asthma_lut_v1_exclusions.csv")

# Remove the dot from the code list file so we can link to HES
lut_exclusion[, code := gsub("\\.", "", code)]

# Define exclusion variable
diagnoses[, exclude := F]

# Here we have 3-character and 4-character codes to deal with.
diagnoses[substr(code, 1, 3) %in% lut_exclusion[nchar(code) == 3]$code, exclude := T]
diagnoses[substr(code, 1, 4) %in% lut_exclusion[nchar(code) == 4]$code, exclude := T]

# Aggregate to episode level
diagnoses[, exclude_episode := as.logical(max(exclude)), by = .(token_person_id, epistart)]

# Now drop the asthma episodes where exclude_episode is TRUE.
diagnoses <- diagnoses[exclude_episode == F]

# Clean up the dataset to save memory. We do not actually need to retain any
# of the new flags as they are now all valid asthma episodes. (This step is not
# really necessary in this example as the dataset is so small, but you may wish
# to consider it in real-world analyses.)

# consider renaming this data.table asthma_episodes? #
diagnoses <- diagnoses[, c("token_person_id",
                           "epikey",
                           "epistart")]

diagnoses <- diagnoses[!duplicated(diagnoses)]


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

spine[, ever_asthma := token_person_id %in% diagnoses$token_person_id]

# Remove the temporary data from memory
rm(diagnoses, diag_cols, lut, lut_exclusion, hes_2019)

# We now have some binary flags that indicate the presence of a code in this year.
table(spine$ever_asthma, useNA = "always")
