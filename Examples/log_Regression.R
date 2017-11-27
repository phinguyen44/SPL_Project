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

df.out = data.frame(df.out)

#Selection of variables for regression
variables = c("ch021_mod" ,"siblings_alive", "euro1", "bmi2", "female","country", "age", "eduyears_mod", "sphus", "chronic_mod", "casp", "maxgrip", "thinc_m", "ep005_", "br015_") 
df.var = df.out[variables]

# No activity dummy

dummy.no.activity = {df.var$br015_ == 3 | df.var$br015_ ==4}
no.activity = data.frame(ifelse(dummy.no.activity == TRUE, 1, 0))


# Dummy unhappy: lower 30%
unhappy.benchmark = quantile(df.out$casp, c(0.3), na.rm = TRUE)
dummy_unhappy = df.var$casp <= unhappy.benchmark
unhappy = data.frame(ifelse(dummy_unhappy == TRUE, 1, 0))

# Dummy happy: happy 30%
happy.benchmark = quantile(df.out$casp, c(0.7), na.rm = TRUE)
dummy_happy = df.var$casp >= happy.benchmark
happy = data.frame(ifelse(dummy_happy == TRUE, 1, 0))

# Dummy depression
dummy_depression = df.var$euro1 == 1
depression = data.frame(ifelse(dummy_depression == TRUE, 1, 0))



df.reg = cbind(df.var, unhappy, happy, no.activity, depression)

colnames(df.reg) = c("num.grandch" ,"siblings_alive", "depression", "bmi2", "female","country", "age", "eduyears_mod", "sphus", "chronic_mod", "casp", "maxgrip", "thinc_m", "ep005_" ,"br015_", "unhappy", "happy" , "no.activity") 

df.reg$sphus = as.factor(df.reg$sphus)
df.reg$bmi2 = as.factor(df.reg$bmi2)
df.reg$country = as.factor(df.reg$country)



######### Logistic Regression ########
#Section Input: List of possible determinants of wellbeing 'df.reg' and unhappy dummy

#Regression for unhappy
RegModel= glm(unhappy ~  depression + num.grandch +chronic_mod  + thinc_m + sphus + no.activity, data = df.reg,
                         family = binomial())

RegModel2= glm(unhappy ~  depression, data = df.reg,
              family = binomial())

               
#Regression for happy
#RegModel1 = glm(happy ~ age + sphus + bmi2 + no.activity, data = df.reg, 
              # family = binomial())

# Function summarizing log regression results
result.reg = function()
summary(RegModel)
exp(RegModel$coefficients) 

#install.packages("car") install package if necessary
library(car)
vif(RegModel) # check for multi collinearity 


#Misclassification Error
misClassError(df.reg$unhappy, predicted, threshold = 0.5) # % of misclassified values in total 

# install.packages("InformationValue") install package if necessary
library(InformationValue)
optCutOff <- optimalCutoff(df.reg$unhappy, predicted)[1] 

predicted = plogis(predict(RegModel, df.reg))  # predicted scores
predicted = data.frame(predicted)

predicted1 = data.frame(ifelse(predicted$predicted >= .5, 1, 0))

predicted1 = cbind(predicted1, df.reg$unhappy)
colnames(predicted1) = c("predict.value", "true.value")


#Mispecification
K = which(predicted1$predict.value == 1 & predicted1$true.value == 1)
L = which(predicted1$predict.value == 1 & predicted1$true.value == 0)

summary(dummy_unhappy)

length(K)/19926 #-> 44% of unhappy predicted correctly 
length(L)/41931 # 9% of happy people predicted to be unhappy 

(length(K)+ length(L))/(66221-4364) #mispecificaition error


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
