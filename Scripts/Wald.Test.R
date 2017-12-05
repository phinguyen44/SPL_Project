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
  
  # Specify the of coefficients to be tested: only health variable
  health = c(16:19)
  
  # Test only the joint significance of health variables
  # TODO: select health coefficients in a more efficient manner -> some Models have less coefficients
  testOutput = try(wald.test(b = coef(ModelElement), Sigma = vcov(ModelElement), Terms = health)$result)
  
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

wald.bound = t(as.data.frame(wald.log))
modelNames = names(allModels)
wald.df = data.frame(modelNames, wald.bound)


############ Create our own Wald Test ###############

############ First get results from waldtest packes for female Austrian ###########

library(aod)
library(numDeriv)

## Multivariate Case for Wald Test

R = matrix(0, nrow = 23, ncol = 23)
coef.H0 = c(16:19) # Coefficients to be tested
# Assign restrictions to R and r
for (i in min(coef.H0):max(coef.H0)){ R[i,i] = 1} 
r = rep(0, length(coef.H0))
theta = allSummaries$AUT.FEMALE$coefficients

R %*% b
#=> our test statistics reduces to:

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
# make Wald Test more general for all hypothesis? 

sigma_est = ((allSummaries$AUT.FEMALE$coefficients[,2])%*%
             t(allSummaries$AUT.FEMALE$coefficients[,2]))/ length(allSummaries$AUT.FEMALE$coefficients[,2])

X = as.matrix(allModels$AUT.FEMALE$data)
XX_inv = solve(t(X) %*% X)

Var.theta = sigma_est %*% XX_inv


######### JUST SOME NOTES #########


t(allSummaries$AUT.FEMALE$cov.unscaled)%*%
  (allSummaries$AUT.FEMALE$df.residual)


## Univariate Case
# W = (theta_est - theta_zero)^2 / Var_est(theta_est)
# Calculation Z value from coefficients and standard error
# for H0: all b equal to zero: z = (theta_est - 0) / se 

# Example
#Z = 0.21302 / 0.45758  
#p.value = 2*(1-pt(abs(Z),nrow(allModels$AUT.FEMALE$data)-ncol(allModels$AUT.FEMALE$data)))
#results<-cbind(Z,p.value)
