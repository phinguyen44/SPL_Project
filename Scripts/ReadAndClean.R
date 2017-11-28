################################################################################
# ReadAndClean.R
#
################################################################################
# Description:
# Load data set and remove missing values
# 
################################################################################

################################################################################
# LOAD DATA

rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("USERPROFILE"),"/splrepo/SPL_Project")
setwd(wd)

# Define required packages and install them if necessary. 
neededPackages = c("dplyr", "tidyr", "purrr", "ggplot2", "countrycode", "utils")
allPackages    = c(neededPackages %in% installed.packages()[,"Package"]) 

if(!all(allPackages)) {
  # Find missing packages
  missingIDX = which(allPackages == FALSE) # Retrieve index of missing packages
  lapply(missingIDX, install.packages) # Install packages which aren't found.
}


# Detach all loaded Packages
choiceV = c("continue", "stop execution")
titleV = c("Cleaning Session. Detach all Packages?!")
userChoice = utils::menu(choices = choiceV, graphics = TRUE, title = titleV)


# Check if any packages are loaded
nPackages = length(names(sessionInfo()$otherPkgs))

# Only need to detach any Package if at least one is loaded
if(userChoice == 1 & nPackages != 0) {
  lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
         detach, character.only = TRUE, unload = TRUE)
  warning("Cleaning Session!\nAll packages are being detached!")  
} else {
  warning("You are proceeding without a clean session. ")
}

# Load all defined packages
lapply(neededPackages, function(z) library(z, character.only = TRUE))

# Load dataset
load("Data/easySHARE_rel6_0_0.rda")
dat.input = easySHARE_rel6_0_0 # always use unique dataset input
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

# if working hours is NA, this means individuals don't work
df$ep013_mod[is.na(df$ep013_mod)] = 0
# crosscheck this with variable ep005

# Create dummies and clean naming conventions for ease
country_list = c("BEL", "NLD", "FRA", "SWE", "DEU", "GRC", "ITA", "ESP", "DNK",
                 "AUT", "CHE")
country_data = with(countrycode_data, data.frame(iso3c, iso3n)) # country code
df.out       = df %>% 
    dplyr::left_join(country_data, by = c("country_mod" = "iso3n")) %>% 
    dplyr::mutate(country       = iso3c,
                  age           = floor(age),
                  age50_54      = age < 55,
                  age55_59      = age >= 55 & age < 60,
                  age60_64      = age >= 60,
                  edu_low       = isced1997_r %in% c(0, 1),
                  edu_second    = isced1997_r %in% 2:4,
                  edu_high      = isced1997_r %in% c(5, 6),
                  children      = ch001_,
                  married       = mar_stat %in% 1:3,
                  h_chronic     = chronic_mod,
                  h_maxgrip     = maxgrip,
                  h_adla        = adla > 0,
                  h_overweight  = bmi2 == 3,
                  h_obese       = bmi2 == 4, 
                  h_badmental   = eurod > 3,
                  h_goodsp      = sphus < 4,
                  labor_supply  = ep013_mod,
                  labor_ft      = ep013_mod > 32,
                  labor_pt      = ep013_mod < 32 & ep013_mod > 0,
                  labor_np      = ep013_mod == 0) %>% 
  dplyr::filter(country %in% country_list) %>% 
  dplyr::select(wave, country,              # wave and country
                female, children, married,  # demographic details
                starts_with("age"),         # age dummy
                starts_with("edu_"),        # eduction dummies
                starts_with("h_"),          # health indicators
                starts_with("labor_")) %>%  # labor supply outcomes 
  na.omit() # remove missing values

# TODO: create a function that can read a df and print out relevant statistics (e.g. num rows dropped bc/ na, etc. etc, et.c)

# TODO: don't forget to standardize numeric variables: children, grip strength, health_cond! (do that just for estimation step)
         
# TODO: all dummies should be of the same type
#-> e.g. femal is now numeric wheresas age 50_54 is logical


rm(list = ls()[ls() != "df.out"])

# create standardized variables for numeric data

standardize = function(x){
  mean = sum(x)/length(x)
  std = sd(x)
val  = (x - mean) / std
return(val)
}
# Comment for report: mention that gives same results as inbuilt function (scale)

idx = sapply(df.out, is.numeric)
idx = seq(1:length(idx))[idx]
# gives a vector of integer column positions of numeric variables 


# Creating separate data set with standardized numeric variables for regression
df.reg = df.out %>% 
  mutate_at(.vars = vars(idx), 
            .funs = standardize)

# TO DO: declare wave as categorical variable -> otherwise will be standadized

rm(list= ls()[!(ls() %in% c('df.out','df.reg'))])

