setwd("/Users/claudiaguenther/Documents/Studium/MEMS/WS2017:18/SPL/Quantlets")
getwd()
rm(list = ls())
load("easySHARE_rel6_0_0.rda")

# In case you have not yet installed the package "data.table" execute the following command:
# install.packages("data.table")
# now continue with the library
library("data.table")
# data.table is needed for copy. 
# I recommend to read a little about it due to its simple grouping features


dat = copy(easySHARE_rel6_0_0)

a = c(-2, -3, -4, -7, -9, -12, -13, -14, -15, -16)
b = c("tocheck","implausible", "tocheck","uncoded", "notApplicable", "dontKnow", "notAskedWave", "notAskedCountry", "noInformation", "noDropOff")
missing.value.codes = data.frame(a,b)

# Wave 1 ONLY
df = subset(dat, dat$wave == "1")

df$country = as.factor(df$country)
levels(df$country)

df = subset(df, df$country != 25) #exclude Israel


# Numeric:
# All only first three columns are characters, rest is integers or numeric. No conversion necessary


# Find NA locations and declare them as such
df.out= apply(df[,-c(1:3)], 2, function(z){
  
  na.loc = which(z %in% a)
  z[na.loc] = NA
  
  return(z)
  
})

# Section Output: df.out, object: (Large) matrix! Coerce to data table if desired in the following sections.

df.out = data.frame(df.out)

summary(df.out)


f = df.out[, c("age", "sphus", "ch001_", "partnerinhh", "eurod", "bmi") ]
f = f[complete.cases(f),]
f$sphus = as.numeric(f$sphus)
hist(f$sphus)
boxplot(f$sphus)

dat$mergeid = as.factor(dat$mergeid)
str(dat$mergeid)
