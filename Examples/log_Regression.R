setwd("/Users/claudiaguenther/Documents/Studium/MEMS/WS2017:18/SPL")
getwd()
rm(list = ls())
load("/Users/claudiaguenther/Downloads/easySHARE_6.0.0_R.zip/easySHARE_rel6_0_0.rda")

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

# Wave 5 ONLY
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

# Age Filter // not useful
#age_45_65 = data.frame(ifelse({df.out$age >= 45 & df.out$age <= 65}, TRUE, FALSE))
#df.out = df.out[age_45_65 == TRUE,]

#Selection of variables for regression
variables = c("q34_re", "iv009_mod", "partnerinhh", "ch007_km","ch007_hh", "ch001_" ,"income_pct_w5", "ch021_mod" ,"siblings_alive", "euro1", "eurod" , "bmi2", "female","country", "age", "eduyears_mod", "sphus", "chronic_mod", "casp", "maxgrip", "thinc_m", "ep005_", "br015_") 
df.var = df.out[variables]

# No activity dummy
dummy.no.activity = {df.var$br015_ == 3 | df.var$br015_ ==4}
no.activity = data.frame(ifelse(dummy.no.activity == TRUE, 1, 0))


# Dummy unhappy: lower 30%
unhappy.benchmark = quantile(df.out$casp, c(0.3), na.rm = TRUE)
dummy_unhappy = df.var$casp <= unhappy.benchmark
unhappy = data.frame(ifelse(dummy_unhappy == TRUE, 1, 0))

summary(dummy_unhappy)

# Dummy happy: happy 30%
happy.benchmark = quantile(df.out$casp, c(0.7), na.rm = TRUE)
dummy_happy = df.var$casp >= happy.benchmark
happy = data.frame(ifelse(dummy_happy == TRUE, 1, 0))

# Dummy depression
dummy_depression = df.var$euro1 == 1
depression.dummy = data.frame(ifelse(dummy_depression == TRUE, 1, 0))


# Dummy children
dummy_child = df.var$ch001_ > 0
child.dummy = data.frame(ifelse(dummy_child == TRUE, 1, 0))


# Age groups
#age1 = data.frame(ifelse({df.var$age < 45}, "below.45", 0))
#age2 = data.frame(ifelse({df.var$age >= 45 & df.var$age <= 55 }, "age.45.55", 0))
#age3 = data.frame(ifelse({df.var$age > 55 & df.var$age <= 65 }, "age.55.65", 0))
#age4 = data.frame(ifelse({df.var$age > 65 & df.var$age <= 75 }, "age.65.75", 0))
#age5 = data.frame(ifelse({df.var$age > 75 & df.var$age <= 85 }, "age.65.75", 0))
#age6 = data.frame(ifelse({df.var$age > 85}, "above.85", 0))


### Create dataframe for regression

df.reg = cbind(df.var, unhappy, no.activity, depression.dummy, child.dummy)

colnames(df.reg) = c("religion", "area","partnerinhh", "child.near","child.in.hh", "num.children", "income.percentile", 
                     "num.grandch" ,"siblings_alive", "depression", "euroindex" , "bmi2", "female","country", 
                     "age", "eduyears_mod", "sphus", "chronic_mod", "casp", "maxgrip", "thinc_m", 
                     "ep005_" ,"br015_", "unhappy" , "no.activity", "depression.dummy", "child.dummy") 

# This should be done in data import section
df.reg$sphus = as.factor(df.reg$sphus)
df.reg$bmi2 = as.factor(df.reg$bmi2)
df.reg$country = as.factor(df.reg$country)
df.reg$child.near = as.factor(df.reg$child.near)
df.reg$child.in.hh = as.factor(df.reg$child.in.hh)
df.reg$ep005_ = as.factor(df.reg$ep005_)
df.reg$bmi2 = as.factor(df.reg$bmi2)
df.reg$income.percentile = as.factor(df.reg$income.percentile)
df.reg$eduyears_mod = as.factor(df.reg$eduyears_mod)
df.reg$partnerinhh = as.factor(df.reg$partnerinhh)
df.reg$religion = as.factor(df.reg$religion)
df.reg$area = as.factor(df.reg$area)


rm(df.out, dummy_child, depression.dummy, happy, no.activity, unhappy, child.dummy)
rm( a, b, dat, df, missing.value.codes, 
    dummy_depression, dummy_happy, dummy_unhappy, dummy.no.activity)

######### Logistic Regression ########
#Section Input: List of possible determinants of wellbeing 'df.reg' and unhappy dummy

#Regression for unhappy
RegModel= glm(unhappy ~  + euroindex + ep005_ + partnerinhh + child.near +
                country  + child.in.hh +  female + child.dummy  +chronic_mod  + 
                + age + bmi2 + thinc_m + sphus + no.activity,
                data = df.reg,
                         family = binomial())

#problem when adding partnerinhh: Algorithmus konvergierte nicht 

RegModel2= glm(unhappy ~  depression, data = df.reg,
              family = binomial())

               
#Regression for happy
#RegModel1 = glm(happy ~ age + sphus + bmi2 + no.activity, data = df.reg, 
              # family = binomial())

# Function summarizing log regression results
summary(RegModel)
exp(RegModel$coefficients) 

#install.packages("car") install package if necessary
library(car)
vif(RegModel) # check for multi collinearity 




# install.packages("InformationValue") install package if necessary
library(InformationValue)

predicted = plogis(predict(RegModel, df.reg))  # predicted scores
predicted = data.frame(predicted)

optCutOff <- optimalCutoff(df.reg$unhappy, predicted)[1] 


predicted1 = data.frame(ifelse(predicted$predicted >= .5, 1, 0))

predicted1 = cbind(predicted1, df.reg$unhappy)
colnames(predicted1) = c("predict.value", "true.value")
#Misclassification Error
misClassError(df.reg$unhappy, predicted, threshold = 0.5) # % of misclassified values in total 

#Mispecification
K = which(predicted1$predict.value == 1 & predicted1$true.value == 1)
L = which(predicted1$predict.value == 1 & predicted1$true.value == 0)

length(K)/19926 #-> 45% of unhappy predicted correctly 
length(L)/41931 # 8% of happy people predicted to be unhappy 



# ROC plot: races the percentage of true positives accurately predicted by a given logit model as the prediction probability cutoff is lowered from 1 to 0
plotROC(df.reg$unhappy, predicted)

# Concordance
Concordance(df.reg$unhappy, predicted)

# Some further model evaluation possibilities
sensitivity(df.reg$unhappy, predicted, threshold = 0.5)
specificity(df.reg$unhappy, predicted, threshold = 0.5)

confusionMatrix(df.reg$unhappy, predicted, threshold = 0.5)

#install.packages("Information")
library(Information)

IV <- create_infotables(data=df.reg, y=unhappy, bins=4)


# To Dos for regression Quantlet

# define structure of quantlet 
# Input: Data with imputed values
# Output: Model coefficients and model specification indicators
# Define functions
    # for dummy creation
    # maybe based on general SHARE database structure



