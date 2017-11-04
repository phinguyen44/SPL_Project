##################################################################################################
# EDA.R
#
##################################################################################################
# Description:
# Some basic exploratory analysis. Plots, tables, univariate correlations.
# 
##################################################################################################

##################################################################################################
# LOAD DATA

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/SPL_Project")
setwd(wd)

# Source cleaned data set
source("Scripts/ReadAndClean.R")

# Get necessary packages for analysis
# run install.packages("needs") and library(needs) if not installed yet
needs(ggplot2, tidyr, dplyr, zoo, corrplot, countrycode)

##################################################################################################
# Organize Data Set

# first pass: select only those that have theoretical value
base      = c("female", "age")
location  = c("country_mod", "birth_country", "iv009_mod")
education = c("isced1997_r", "eduyears_mod")
family    = c("mar_stat", "hhsize", "ch001_", "ch021_mod", "ch007_km") 
health    = c("chronic_mod", "eurod", "bmi", "smoking", "br010_mod", "br015_")
job       = c("ep005_", "ep013_mod", "ep026_mod", "co007_", "thinc_m")

yvar      = "casp"

df.slim = df.out %>% 
  select(base, location, education, family, health, job) %>% 
  rename(base_gender        = female,
         base_age           = age,
         loc_country        = country_mod,
         loc_birth          = birth_country,
         loc_area           = iv009_mod,
         edu                = isced1997_r,
         edu_years          = eduyears_mod,
         fam_married        = mar_stat,
         fam_hhsize         = hhsize,
         fam_num_child      = ch001_,
         fam_num_grandchild = ch021_mod,
         fam_child_prox     = ch007_km,
         health_disease     = chronic_mod,
         health_depression  = eurod,
         health_bmi         = bmi,
         health_smoking     = smoking,
         health_drinking    = br010_mod,
         health_activities  = br015_,
         job_status         = ep005_,
         job_hours          = ep013_mod,
         job_satisfaction   = ep026_mod,
         job_afford         = co007_,
         job_income         = thinc_m)

# gonna have to do some imputation or remove some variables because only 1031 complete cases

# discretize some variables?
# regularize?

