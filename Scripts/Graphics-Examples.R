################################################################################
# Graphics-Examples.R
#
################################################################################
# Description:
# Create useful graphics. Quantlet enhances report by including numeric 
# variables for which information is lost in conversion into dummary variables.
# 
# Functions are designed to work with df.out created from `ReadAndClean.R`
# 
################################################################################

################################################################################
# SET WORKING DIRECTORY

# Note: Only this part must be changed for the rest of the script to run.
rm(list = ls())

# Adjust your working directory to where your local repository is located
wd = file.path("~/Documents/Projects/SPL_Project")
setwd(wd)

################################################################################
# SOURCE DATA

source("Scripts/ReadAndClean.R")
source("Scripts/Graphics.R")
datasets = read.and.clean(dataset = "easySHARE_rel6_0_0.rda")

#Only keep relevant data sets
df.out = datasets$df.out
rm(datasets)

################################################################################
# LOAD NECESSARY PACKAGES & DATA

# List all packages needed for session
neededPackages = c("dplyr", "magrittr", "ggplot2", "scales")
allPackages    = c(neededPackages %in% installed.packages()[ , "Package"]) 

# Install packages (if not already installed) 
if(!all(allPackages)) {
    missingIDX = which(allPackages == FALSE)
    needed     = neededPackages[missingIDX]
    lapply(needed, install.packages)
}

# Load all defined packages
lapply(neededPackages, library, character.only = TRUE)

################################################################################
# EXAMPLES

health.distribution('age')
health.distribution('age', gen = 'FEMALE')

health.distribution('h_chronic', gen = 'MALE', remove.outliers = FALSE)

countries = c('Germany', 'Switzerland', 'Austria')
health.distribution('children', gen = 'FEMALE', countries = countries)

health.distribution('h_maxgrip', gen = 'MALE')

health.distribution('h_depression')

health.distribution('h_perceived', remove.outliers = FALSE)

ggsave("Output/healthdistribution.png", plot = last_plot(), width = 8, height = 5, units = "in")
