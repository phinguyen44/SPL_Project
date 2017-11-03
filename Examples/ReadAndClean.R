
# Adjust your working directory
wd = file.path(Sys.getenv("USERPROFILE"),"/Desktop/StatistikMaster/SPL")
setwd(wd)

# In case you have not yet installed the package "data.table" execute the following command:
# install.packages("data.table")
# now continue with the library
library("data.table")
# data.table is needed for copy. 
# I recommend to read a little about it due to its simple grouping features

load("easySHARE_rel6_0_0.rda")
dat = copy(easySHARE_rel6_0_0)

a = c(-3, -7, -9, -12, -13, -14, -15, -16)
b = c("implausible", "uncoded", "notApplicable", "dontKnow", "notAskedWave", "notAskedCountry", "noInformation", "noDropOff")
missing.value.codes = data.frame(a,b)

# Wave 6 ONLY
df = subset(dat, dat$wave == "5")

# Numeric:
# All only first three columns are characters, rest is integers or numeric. No conversion necessary


# Find NA locations and declare them as such
df.out= apply(df[,-c(1:3)], 2, function(z){
  
  na.loc = which(z %in% a)
  z[na.loc] = NA
  
  return(z)
  
})

# Section Output: df.out, object: (Large) matrix! Coerce to data table if desired in the following sections.

