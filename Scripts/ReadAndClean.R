################################################################################
# ReadAndClean.R
#
################################################################################
# Description:
# Load data set and remove missing values
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
# LOAD NECESSARY PACKAGES & DATA

# List all packages needed for session
neededPackages = c("dplyr", "tidyr", "ggplot2", "magrittr", "countrycode")
allPackages    = c(neededPackages %in% installed.packages()[,"Package"]) 

# Install packages (if not already installed) 
if(!all(allPackages)) {
  missingIDX = which(allPackages == FALSE)
  needed     = neededPackages[missingIDX]
  lapply(needed, install.packages)
}

# Load all defined packages
lapply(neededPackages, library, character.only = TRUE)

# Load dataset
load("easySHARE_rel6_0_0.rda")
dat.input = easySHARE_rel6_0_0 
rm(easySHARE_rel6_0_0)

################################################################################
# ENCODE MISSING VALUES

# Organize data.frame by selecting relevant variables
dat = dat.input %>% 
  filter(wave == "1" & (age <= 64 & age >= 50)) %>%  # wave 1 & age filter
  select(wave, country_mod,                          # dataset details
         female, age, isced1997_r, ch001_, mar_stat, # demographic variables
         chronic_mod, maxgrip, adla, bmi2, eurod, sphus, # health indicators
         ep013_mod)                                  # labor (outcome var)

rm(dat.input)

# Encode missing values according to SHARE dataset guidelines
a = c(-2, -3, -4, -7, -9, -12, -13, -14, -15, -16)
b = c("tocheck","implausible", "tocheck", "uncoded", "notApplicable", 
      "dontKnow", "notAskedWave", "notAskedCountry", "noInformation", 
      "noDropOff")
missing.value.codes = data.frame(a,b) 
# This data frame can be used to verify NA codes easily. 
# But for the encoding only the numeric vector a is necessary.

# Find NA locations and declare them as such
df.decl = apply(dat, 2, function(z) {
  na.loc    = which(z %in% a)
  z[na.loc] = NA
  return(z)
})

df = data.frame(df.decl) 

# If working hours is NA, this means individuals don't work
df$ep013_mod[is.na(df$ep013_mod)] = 0
# TODO: crosscheck this with variable ep005

################################################################################
# CREATE DATA FRAMES FOR ANALYSIS AND ESTIMATION

# Get country information from "countrycode" package
country_list = c("BEL", "NLD", "FRA", "SWE", "DEU", "GRC", "ITA", "ESP", "DNK",
                 "AUT", "CHE")
country_data = with(countrycode_data, data.frame(iso3c, iso3n))

# Variables are cleaned, converted into human-readable naming conventions, and
# converted to dummies as described in the paper
df.out       = df %>% 
    dplyr::left_join(country_data, by = c("country_mod" = "iso3n")) %>% 
    dplyr::filter(iso3c %in% country_list) %>% 
    dplyr::mutate(country       = factor(iso3c),
                  gender        = factor(ifelse(female, "FEMALE", "MALE")),
                  age50_54      = age < 55,
                  age55_59      = age >= 55 & age < 60,
                  age60_64      = age >= 60,
                  age           = factor(floor(age)),
                  edu_low       = isced1997_r %in% c(0, 1),
                  edu_second    = isced1997_r %in% 2:4,
                  edu_high      = isced1997_r %in% c(5, 6),
                  children      = ch001_,
                  couple        = mar_stat %in% 1:3,
                  h_chronic     = chronic_mod,
                  h_maxgrip     = maxgrip,
                  h_adla        = adla > 0,
                  h_overweight  = bmi2 == 3,
                  h_obese       = bmi2 == 4, 
                  h_badmental   = eurod > 3,
                  h_goodsp      = sphus < 4,
                  labor_ft      = ep013_mod > 32,
                  labor_pt      = ep013_mod < 32 & ep013_mod > 0,
                  labor_np      = ep013_mod == 0) %>%
    dplyr::select(country, gender,              # country and gender
                  starts_with("age"),           # age dummy
                  starts_with("h_"),            # health indicators
                  starts_with("edu_"),          # eduction dummies
                  children, couple,             # demographic details
                  starts_with("labor_")) %>%    # labor supply outcomes 
    na.omit() %>%                               # remove missing values
    set_rownames(NULL)                          # reset row numbering
    
# TODO: create a function that can read a df and print out relevant statistics 
# (e.g. num rows dropped bc/ na, etc. etc, et.c). Or generalize so separate
# variables can be used. (e.g. create a standard "clean" function for working 
# with the SHARE data set)
# TODO: determine threshold for "severe" and "mild" conditions (currently we
# just set it as numeric

# Create standardized variables for numeric data
standardize = function(x) {
    mean = sum(x)/length(x)
    std  = sd(x)
    val  = (x - mean) / std
    return(val)
}
# TODO: Comment in report: mention it gives same results as inbuilt function 
# (scale)

# Gives a vector of integer column positions of numeric variables 
idx = sapply(df.out, is.numeric)
idx = seq(1:length(idx))[idx]

# Creating separate data set with standardized numeric variables for regression,
# then reselect variables as described in paper (e.g. self-reported health is 
# removed)
df.reg = df.out %>% 
    mutate_at(.vars = vars(idx), 
              .funs = standardize) %>% 
    mutate(labor_participation = !labor_np) %>% # invert to get labor_part rate
    select(country, gender, age, 
           h_chronic, h_adla, h_obese, h_maxgrip,
           edu_second, edu_high, children, couple,
           labor_participation)
    
# Create a list of data frames by country and gender, to be used in regression
df.splits  = split(df.reg, f = list(df.reg$country, df.reg$gender), drop = TRUE)

# Create necessary dummary variables for regression
dummify = function(data.frame) {
    data.frame = data.frame %>% 
        select(-country, -gender)                # remove country/gender
    model      = ~ 0 + .                         # needed to remove intercept
    new.df     = model.matrix(model, data.frame) # create dummies
    new.df     = data.frame(new.df)
    return(new.df)
}

df.splits = lapply(df.splits, dummify)

# df.out is for analysis, df.reg and df.splits are for estimation
rm(list= ls()[!(ls() %in% c("df.out", "df.splits", "df.reg"))])
