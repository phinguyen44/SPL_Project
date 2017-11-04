##################################################################################################
# SPL Project
# 
##################################################################################################
# Exploratory Data Analysis
# 
##################################################################################################

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/SPL_Project")
setwd(wd)

# Load data
load("data/easySHARE.rda")
dat = easySHARE_rel6_0_0

# Get necessary packages for analysis
# run install.packages("needs") and library(needs) if not installed yet
needs(ggplot2, tidyr, dplyr, zoo, corrplot)

##################################################################################################
# Data Cleaning
# Simple case: convert values to NA if indicated as such in the data dictionary provided

# Set missing value codes
a = c(-3, -7, -9, -12, -13, -14, -15, -16)
b = c("implausible", "uncoded", "notApplicable", "dontKnow", "notAskedWave", "notAskedCountry", "noInformation", "noDropOff")
missing.value.codes = data.frame(a,b)

# Wave 5 ONLY
df = subset(dat, dat$wave == 5)

# Find NA locations and declare them as such
df.out = apply(df[,-c(1:3)], 2, function(z){
  na.loc    = which(z %in% a)
  z[na.loc] = NA
  
  return(z)
})

df.out = data.frame(df.out)

##################################################################################################
# Organize Data Set

# first pass: select only those that have theoretical value
base = c("country_mod", "female", "age", "birth_country", "iv009_mod", "q34_re")
education = c("isced1997_r", "eduyears_mod")
family = c("mar_stat", "ch001_", "ch007_km")
social
health = c("chronic_mod", "euro1", "bmi", "br010_mod") # do something with the euro vars, maybe compare to one another
job = c("ep005_", "ep026_mod", "thinc_m", )
# add something about health TESTS

yvar = "casp"

df.slim = 

# discretize some variables?

