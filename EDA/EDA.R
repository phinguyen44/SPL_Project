#########################################################################################################
# SPL Project
# 
#########################################################################################################
# Exploratory Data Analysis
# 
#########################################################################################################

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/SPL_Project")
setwd(wd)

# Load data
load("data/easySHARE.rda")
df = easySHARE_rel6_0_0

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