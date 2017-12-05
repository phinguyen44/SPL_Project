################################################################################
# Wald.Test
#
################################################################################
# Description:
# 
# 
################################################################################

################################################################################

# Comment: How should we start this quantlet? Load the pre-processd data set?
# Specify with data must be loaded? 


rm(list= ls()[!(ls() %in% c("df.reg", "df.splits"))])


################################################################################
# LOAD NECESSARY PACKAGES & DATA

# List all packages needed for session
neededPackages = c("aod")
allPackages    = c(neededPackages %in% installed.packages()[,"Package"]) 

# Install packages (if not already installed) 
if(!all(allPackages)) {
  missingIDX = which(allPackages == FALSE)
  needed     = neededPackages[missingIDX]
  lapply(needed, install.packages)
}

# Load all defined packages
lapply(neededPackages, library, character.only = TRUE)


################################################################################

# Probit for each country and gender 
allModels = lapply(df.splits, function(z){
  
  z = z[-z$age50] # Multicollinearity
  
  model = glm(z$labor_participationTRUE ~., family = binomial(link = "probit"), data = z)
  
  return(model)
  
})


# Return summaries
allSummaries = lapply(allModels, summary)


# Wald Test

wald.log = list() # Save Wald Test Output

for(i in 1:length(allModels)){
  
  # Get Element
  ModelElement = allModels[[i]]
  
  # Specify number of coefficients: Columns - 1 (dependent Variable)
  nTerms = ncol(ModelElement$data) - 1
  
  testOutput = try(wald.test(ModelElement, terms = nTerms))
  
  if(class(testOutput) == "try-error"){
    
    # Display warning and investigate
    msg = paste0("Wald Test failed for Model Element ", i)
    warning(msg)
    
    wald.log[[i]] = "Error"
    
    next
    
  } else{
    
    wald.log[[i]] = testOutput
    next
    
  }
  
  rm(ModelElement) # clean up
  
  next
  
}

wald.bound = do.call("rbind.data.frame", wald.log)
modelNames = names(allModels)
wald.df = data.frame(modelNames, wald.bound)

############ Create our own Wald Test ###############

############ First get results from waldtest packes for female Austrian ###########

install.packages("aod")
library(aod)
library(numDeriv)

## Multivariate Case
# W = (theta_est - theta_zero)' [Var_est(theta_est)]^(-1)*theta_est - theta_zero)
#   = (theta_est)' *[Var_est(theta_est)]^(-1)* (theta_est)


# Example for Austrain women
allSummaries$AUT.FEMALE

theta_est_t= t(allSummaries$AUT.FEMALE$coefficients[16:19,1])
theta_est  = allSummaries$AUT.FEMALE$coefficients[16:19,1]
Var_est_theta_inv = solve(vcov(allSummaries$AUT.FEMALE)[16:19, 16:19])

W = theta_est_t %*% Var_est_theta_inv %*% theta_est

pchisq(W, df=23, lower.tail=FALSE)

g = wald.test(b = coef(allModels$AUT.FEMALE), Sigma = vcov(allModels$AUT.FEMALE), Terms = 16:19)
W 
g$result
# => results are equal :) 

########## Construct a general Wald Test ################

# W = t(theta_est) %*% [Var_theta_est]^(-1) %*% theta_est

joint.wald.test = function(theta, model.summary, spec, signf.l){
  joint.wald.test= numeric(3)
  names(joint.wald.test) = c("W","p-value", "df")
  Var_theta_est = vcov(model.summary)
  W = t(theta[spec,1]) %*% solve(Var_theta_est[spec,spec]) %*% theta[spec,1]
  chi2 = qchisq(signf.l, df=length(spec))
  pval = 1-pchisq(W,length(spec))
  joint.wald.test[1] = W 
  joint.wald.test[2] = pval
  joint.wald.test[3] = length(spec)
  joint.wald.test
}

joint.wald.test(allSummaries$AUT.FEMALE$coefficients, allSummaries$AUT.FEMALE, 16:19, 0.95)
# => results are equal :) 

joint.wald.test(allSummaries$CHE.MALE$coefficients, allSummaries$CHE.MALE, 16:19, 0.95)


# TODO:
# Calculate Var_est_theta_inv by hand


######### JUST SOME NOTES #########





## Univariate Case
# W = (theta_est - theta_zero)^2 / Var_est(theta_est)
# Calculation Z value from coefficients and standard error
# for H0: all b equal to zero: z = (theta_est - 0) / se 

# Example
#Z = 0.21302 / 0.45758  
#p.value = 2*(1-pt(abs(Z),nrow(allModels$AUT.FEMALE$data)-ncol(allModels$AUT.FEMALE$data)))
#results<-cbind(Z,p.value)
