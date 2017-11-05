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

# convert country to iso code (alphabetic)
country_data = with(countrycode_data, data.frame(iso3c, iso3n))
df.out       = left_join(df.out, country_data, by = c("country_mod" = "iso3n")) %>% 
  left_join(country_data, by = c("birth_country" = "iso3n"))

# first pass: select only those that have theoretical value (note that NA columns were removed)
base      = c("female", "age")
location  = c("iso3c.x", "iso3c.y", "iv009_mod")
education = c("isced1997_r", "eduyears_mod")
family    = c("mar_stat", "hhsize", "ch001_", "ch021_mod", "ch007_km") 
health    = c("chronic_mod", "eurod", "bmi", "smoking", "br010_mod", "br015_")
job       = c("ep005_", "ep013_mod", "ep026_mod", "co007_", "thinc_m")

yvar      = "casp"

df.slim = df.out %>% 
  select(base, location, education, family, health, job, yvar) %>% 
  rename(base_gender        = female,
         base_age           = age,
         loc_country        = iso3c.x,
         loc_birth          = iso3c.y,
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

df.slim$loc_country = as.character(df.slim$loc_country)
df.slim$loc_birth   = as.character(df.slim$loc_birth)

rm(df.out)

# TODO: IMPUTATION. Only 1031 complete cases otherwise.
# TODO: DISCRETIZATION. Simplify some numeric inputs. Age is a good example.
# TODO: REGULARIZATION. Consider model shrinking methods.

# split data sets into groupings
df.base    = select(df.slim, starts_with("base"), casp)
df.loc     = select(df.slim, starts_with("loc"), casp)
df.edu     = select(df.slim, starts_with("edu"), casp)
df.fam     = select(df.slim, starts_with("fam"), casp)
df.health  = select(df.slim, starts_with("health"), casp)
df.job     = select(df.slim, starts_with("job"), casp)

##################################################################################################
# Data Exploration

# explore happiness by country
casp_country = df.loc %>% 
  group_by(loc_country) %>% 
  summarize(avg_casp = round(mean(casp, na.rm = TRUE),2))



# corrplots for each group
# summary statistics
# creat basic univariate stuff (x[i] vs y)

# mean / histogram of happiness by country (geofacet) + my grid
