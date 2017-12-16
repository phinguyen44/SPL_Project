################################################################################
# Probit.Regression
#
################################################################################
# Description:
# 
# 
################################################################################

################################################################################

# Comment: How should we start this quantlet? Load the pre-processd data set?
# Specify with data must be loaded? 


rm(list= ls())

################################################################################
# SOURCE DATA

source("Scripts/ReadAndClean.R")
datasets = read.and.clean(dataset = "easySHARE_rel6_0_0.rda")

#Only keep relevant data sets
df.splits = datasets$df.splits
rm(datasets)



################################################################################
# LOAD NECESSARY PACKAGES & DATA

# List all packages needed for session
neededPackages = c("aod", "devtools", "margins", "mfx")
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

################################################################################
# Wald Test for joint significance

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


################################################################################

# Wald Test for all models

wald.log = list() # Save Wald Test Output

for(i in 1:length(allSummaries)){
  
  # Get Element
  SummaryElement = allSummaries[[i]]
  
  # Specify the of coefficients to be tested: only health variable
  health = c(16:19)
  
  # Test only the joint significance of health variables
  testOutput = joint.wald.test(allSummaries[[i]], health, 0.95)
  
  if(class(testOutput) == "try-error"){
   
      
# TODO: reformulate error messahe function 
    # Display warning and investigate
    msg = paste0("Wald Test failed for Model Element ", i)
    warning(msg)
    
    wald.log[[i]] = "Error"
    
    next
    
  } else{
    
    wald.log[[i]] = testOutput
    next
    
  }
  
 #TODO: clean up is not working
  rm(SummaryElement) # clean up

  next
  
}

wald.bound = t(as.data.frame(wald.log2))
modelNames = names(allModels)
wald.df = data.frame(modelNames, wald.bound)


################################################################################
# Calculate employment probability

empl.prob = function(model){
    
    # Calculate average person per country & gender 
        X                   = model$data
        X_mean              = data.frame(t(apply(X, 2, mean)))
        
    # Predict probability of being employed of average person
        empl.probability    = predict(object = model, newdata =  X_mean, type = "response")
        return(empl.probability)
}

empl.Models = lapply(allModels, empl.prob)
 
################################################################################
# Calculate marginal effects and standard errors

# Same structure as before, but must calculate model again. 
mfx.Models = lapply(df.splits, function(z){
    
    z = z[-z$age50] # Multicollinearity
    
    res = probitmfx(z$labor_participationTRUE ~.,atmean = TRUE,  data = z)
    
    return(res)
    
})

################################################################################


