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
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/SPL_Project")
setwd(wd)

install.packages("needs")
library(needs)

needs(dplyr, tidyr, purrr, ggplot2, countrycode)

load("Data/easySHARE_rel6_0_0.rda")
dat = easySHARE_rel6_0_0
rm(easySHARE_rel6_0_0)

################################################################################
# ENCODE MISSING VALUES

# Organize data.frame by selecting relevant variables
dat = dat %>% 
  filter(wave == "1" & (age <= 64 & age >= 50)) %>%  # wave 1 & age filter
  select(wave, country_mod,                          # dataset details
         female, age, isced1997_r, ch001_, mar_stat, # demographic variables
         chronic_mod, maxgrip, adla, bmi2, eurod, sphus, # health indicators
         ep013_mod)                                  # labor (outcome var)

# Encode missing values according to SHARE dataset guidelines
a = c(-2, -3, -4, -7, -9, -12, -13, -14, -15, -16)
b = c("tocheck","implausible", "tocheck", "uncoded", "notApplicable", 
      "dontKnow", "notAskedWave", "notAskedCountry", "noInformation", 
      "noDropOff")
missing.value.codes = data.frame(a,b)

# Find NA locations and declare them as such
df = apply(dat, 2, function(z) {
  na.loc    = which(z %in% a)
  z[na.loc] = NA
  return(z)
})

df = data.frame(df) 

# if working hours is NA, this means individuals don't work
df$ep013_mod[is.na(df$ep013_mod)] = 0
# crosscheck this with variable ep005


# Create dummies and clean naming conventions for ease
country_list = c("BEL", "NLD", "FRA", "SWE", "DEU", "GRC", "ITA", "ESP", "DNK",
                 "AUT", "CHE")
country_data = with(countrycode_data, data.frame(iso3c, iso3n))
df.out       = df %>% 
  left_join(country_data, by = c("country_mod" = "iso3n")) %>% 
  mutate(country       = iso3c,
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
  filter(country %in% country_list) %>% 
  select(wave, country,              # wave and country
         female, children, married,  # demographic details
         starts_with("age"),         # age dummy
         starts_with("edu_"),        # eduction dummies
         starts_with("h_"),          # health indicators
         starts_with("labor_")) %>%  # labor supply outcomes 
  na.omit() # remove missing values

# TODO: create a function that can read a df and print out relevant statistics (e.g. num rows dropped bc/ na, etc. etc, et.c)

# TODO: don't forget to standardize numeric variables: children, grip strength, health_cond! (do that just for estimation step)
         

rm(list = ls()[ls() != "df.out"])