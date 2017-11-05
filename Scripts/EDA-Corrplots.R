#################################################################################################
# EDA-Corrplots.R
#
#################################################################################################
# Description:
# Corrplots of variables in data set
#
#################################################################################################

#################################################################################################
# LOAD DATA

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/SPL_Project")
setwd(wd)

source("Scripts/EDA.R")
needs(PerformanceAnalytics)