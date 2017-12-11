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
allModels  = lapply(df.splits, function(z){
  
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
  health       = c(16:19)
  
  # Test only the joint significance of health variables
  # TODO: select health coefficients in a more efficient manner -> some Models have less coefficients
  testOutput   = try(wald.test(b = coef(ModelElement), Sigma = vcov(ModelElement), Terms = health)$result)
  
  if(class(testOutput) == "try-error"){
    
    # Display warning and investigate
    msg        = paste0("Wald Test failed for Model Element ", i)
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

########## Construct a Wald Test for joint significance ################

# W = t(theta_est) %*% [Var_theta_est]^(-1) %*% theta_est

joint.wald.test = function(model.summary, spec, signf.l){
  
  # Define test elements
  joint.wald.test        = numeric(6)
  names(joint.wald.test) = c("Name","W","p-value", "df", "H0" , "Decision")
  beta                   = model.summary$coefficients[,1]
  Var_beta_est           = vcov(model.summary)
  
  # Wald test statistic
  W = t(beta[spec]) %*% solve(Var_beta_est[spec,spec]) %*% beta[spec]
  
  # Set up test output
  chi2               = qchisq(signf.l, df=length(spec))
  pval               = 1-pchisq(W,length(spec))
  joint.wald.test[1] = "Chi2 test"
  joint.wald.test[2] = format(   W, digits = 4) 
  joint.wald.test[3] = format(pval, digits = 4)
  joint.wald.test[4] = length(spec)
  joint.wald.test[5] = "b equal to 0"
  joint.wald.test[6] = ifelse(pval <= 1- signf.l, "Reject H0", "Cannot reject H0")
  joint.wald.test
}

# Check whether Test works
joint.wald.test(allSummaries$AUT.FEMALE, 16:19, 0.95)
wald.test(b = coef(allModels$AUT.FEMALE), Sigma = vcov(allModels$AUT.FEMALE), Terms = 16:19)


# TODO:
# for which model input does function work? just glm model output?
# make Wald Test more general for all hypothesis? 

 ########## Construct a general Wald Test for linear hypothesis ################

# If you want to test special linear hypothesis, define R matrix and r vector

k = 23
R = diag(1, k)
r = rep(0, length(k))

# TO DO: Change test statistic

general.wald.test = function(model.summary, signf.l, R = NULL, r = NULL){
  
  # Define test elements
  general.wald.test        = numeric(6)
  names(general.wald.test) = c("Name","W","p-value", "df", "H0" , "Decision")
  beta                     = model.summary$coefficients[, 1]
  Var_beta_est             = vcov(model.summary)
 
  # Set up restriction matrix/vector for linear hypothesis
  # default option is joint significants of all coefficients
   R  = if (is.null(R)){
    R = diag(1, length(beta)) # default of R is identity matrix 
   } else {
    R = R}
    
  r   = if (is.null(r)){
    r = rep(0, length(beta)) # default for r is null vector
  } else {
    r = r}
  
  # Wald test statistic
  W = t(R%*%beta - r) %*% solve(R%*% Var_beta_est %*% t(R)) %*% (R%*%beta - r)
  
  # Set up test output
  chi2                 = qchisq(signf.l, df=length(r))
  pval                 = 1-pchisq(W,length(r))
  general.wald.test[1] = "Chi2 test"
  general.wald.test[2] = format(   W, digits = 4) 
  general.wald.test[3] = format(pval, digits = 4)
  general.wald.test[4] = length(r)
  general.wald.test[5] = "R*b = r"
  general.wald.test[6] = ifelse(pval <= 1- signf.l, "Reject H0", "Cannot reject H0")
  general.wald.test
}

 
# Test whether it works
B = matrix(0, nrow=2, ncol= 23)
B[1, 4:5] = 1
B[2, 4] = 2
C = c(2,1)
b = allSummaries$AUT.FEMALE$coefficients[,1]

general.wald.test(allSummaries$AUT.FEMALE, 0.95, B, C)


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
