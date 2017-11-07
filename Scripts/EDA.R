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

rm(list = ls())

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

# convert country to iso code (alphabetic)
country_data = with(countrycode_data, data.frame(iso3c, iso3n))
df.out       = left_join(df.out, country_data, by = c("country_mod" = "iso3n")) %>% 
  left_join(country_data, by = c("birth_country" = "iso3n"))

# first pass: select only those that have theoretical value (note that NA columns were removed)
base      = c("female", "age", "eduyears_mod")
location  = c("iso3c.x", "iso3c.y", "iv009_mod")
family    = c("mar_stat", "hhsize", "ch001_", "ch021_mod", "ch007_km") 
health    = c("sphus", "chronic_mod", "eurod", "bmi", "ever_smoked", "br010_mod", "br015_")
job       = c("ep005_", "ep013_mod", "ep026_mod", "co007_", "thinc_m")

yvar      = "casp"

df.slim = df.out %>% 
  select(base, location, family, health, job, yvar) %>% 
  rename(base_gender        = female,         # 1 if female, 0 if male
         base_age           = age,            # age of participant
         base_edu           = eduyears_mod,   # years of education
         loc_country        = iso3c.x,        # location of participant (factor)
         loc_birth          = iso3c.y,        # birth location of participant (factor)
         loc_area           = iv009_mod,      # 1city, 2burbs of city, 3town, 4small town, 5rural
         fam_married        = mar_stat,       # categorical var describing marital status
         fam_hhsize         = hhsize,         # num people in household
         fam_num_child      = ch001_,         # num children
         fam_num_grandchild = ch021_mod,      # num grandchild
         fam_child_prox     = ch007_km,       # 1 if child close, 5 if not (change 5 to 0)
         health_selfperc    = sphus,          # self-perceived health. 1 excellent, 5 poor
         health_disease     = chronic_mod,    # num chronic diseases
         health_depression  = eurod,          # depression scale. 0 not depressed, 12 depressed
         health_bmi         = bmi,            # bmi
         health_smoking     = ever_smoked,    # 1 if smoked, 5 if never (change 5 to 0)
         health_drinking    = br010_mod,      # drinking scale. 1 not at all, 7 daily
         health_activities  = br015_,         # health scale. 1 if often, 4 hardly ever
         job_status         = ep005_,         # 1ret, 2job, 3unemp, 4sick, 5homemaker, 97other
         job_hours          = ep013_mod,      # hours per week worked
         job_satisfaction   = ep026_mod,      # 1 strongly agree - 4 strongly disagree
         job_afford         = co007_,         # can make ends meet. 1 hard - 4 easy
         job_income         = thinc_m)        # household income

rm(df.out)

# TODO: IMPUTATION. Only 1031 complete cases otherwise.
# TODO: DISCRETIZATION. Simplify some numeric inputs. Age is a good example.
# TODO: DROP 97 values from job_status since we don't know what it is

##################################################################################################
# Data Cleaning & Description: describe variables, make necessary changes, summaries.

summary(df.slim)

# Round age to integer
hist(df.slim$base_age)
df.slim$base_age = round(df.slim$base_age)
# TODO: Consider organizing age into discrete buckets.

# Change dummy variable values from 5 to 0
df.slim$fam_child_prox[df.slim$fam_child_prox == 5] = 0
df.slim$health_smoking[df.slim$health_smoking == 5] = 0

# TODO: Fix outliers: fam_num_grandchild, health_bmi

# split data sets into groupings
df.base    = select(df.slim, starts_with("base"), casp)
df.loc     = select(df.slim, starts_with("loc"), casp)
df.fam     = select(df.slim, starts_with("fam"), casp)
df.health  = select(df.slim, starts_with("health"), casp)
df.job     = select(df.slim, starts_with("job"), casp)