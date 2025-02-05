
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Author: Matthew Jay, matthew.jay@ucl.ac.uk
# Code list: emergency_admissions_v1
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

hes_2019 <- data.table(sqlQuery(conn_str, "select top 100000 * from FILE0184861_HES_APC_2019"))
setnames(hes_2019, names(hes_2019), tolower(names(hes_2019)))
em_adm <- fread("codelists/emergency_admissions_v1.csv", stringsAsFactors = F)
rm(conn_str)


# identify emergency admissions -------------------------------------------

# This is a very simple code list that simply uses admimeth values.
# It is used directly in other code lists (see, e.g., the scripts for
# ari_herbert_v1 (adversity-related injuries) and the stress-related presentations
# of Ni Chobhthaigh et al (srp_nichobhthaigh_v2) and Blackburn et al (srp_blackburn_v1)).

hes_2019[, emergency_admission := admimeth %in% em_adm$code]

# Excluding code 2B (transfer of an admitted patient from another hospital
# provider in an emergency)
hes_2019[, emergency_admission := admimeth %in% em_adm[code != "2B"]$code]
