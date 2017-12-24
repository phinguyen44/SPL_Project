################################################################################
# Graphics.R
#
################################################################################
# Description:
# Create useful graphics (Quantlet 6)
# 
# Graphics can be accessed numerous ways.
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
# BUILD GRAPHICS FUNCTION

# TODO: Each is a separate function

###### LABOR PARTICIPATION RATES ACROSS SEGMENTS
# Chloropeth of labor participation rates (men & women) (across age groups) - 6 total graphs. ALLOW FOR SELECTION OF T/F variable

###### VIEW DISTRIBUTION OF NUMERIC VARIABLES
# SELECT: Numeric variable: shows distribution for each country, then overlays average of entire data set (can potentially select one country, otherwise it defaults to all -> maybe pass a list of countries?)
# -> allow slicing by dummy variables

# by default, gender is "all". can either be "all", "MALE" or "FEMALE"
# by default, countries is "all". can also pass a character vector of countries
# by default, filters is "none". 
# - either pass a named logical vector containing predicates from data set
# - or a character vector like c("edu_low = FALSE", "age > 50"). allows for filters outside of just predicates

health.distribution = function(x, 
                               gender    = "all", 
                               countries = "all", 
                               filters   = "none") {
    
    # first convert age to numeric, not factor variable
    df.out$age = as.numeric(levels(df.out$age)[df.out$age])
    
    # STOPPING CONDITIONS
    if (!is.numeric(df.out[[x]])) stop("'x' must be numeric")
    if (length(gender) > 1 | !all(gender %in% c("all", "MALE", "FEMALE"))) {
        stop("'gender' must be one of 'all', 'MALE', or 'FEMALE'")
    }
    if (length(countries) > 1 & !all(countries %in% levels(df.out$country))) {
        stop("'countries' must be 'all' or a character vector containing countries in the `df.out` data frame")
    }
    if (length(countries) == 1) {
        if (!(countries %in% c('all', levels(df.out$country)))) {
            stop("'countries' must be 'all' or a character vector containing countries in the `df.out` data frame")
        }
    }
    # TODO: Add filters stopping condition
    
    # TODO: Figure out how to do filters
    
    # Set faceting variables
    if (gender == "all") gender = c("MALE", "FEMALE")
    if (length(countries) == 1) {
        if (countries == "all") countries = levels (df.out$country)
    }
    
    # p = ggplot(data = df.out, )
    # should it be histogram or barchart?
    
    
}

###### VIEW LABOR CHOICE
# Decide best graph to show. Could be stacked bar, could be bars separate, could be left-side as non working and right-side as working

###### LONGITUDINAL VIEW OF HEALTH
# Labor force participation over time (compare waves) as well as results of counterfactual study

# I like the idea of "tranching" people into categories based on health outcomes. Maybe we'll see something like how really healthy people have amazing labor rates, but unhealthy people have super garbage rates. And that maybe incremental gains from the unhealthy population will lead to higher participation rates, etc. (How to influence policy!) - inequality of health. For that we'll need to run regression on separate buckets of health outcomes