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

# Load own-built Wald tests
source("Scripts/LoadWald.R")

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

# Wald Test for all models

wald.log = list() # Save Wald Test Output

for(i in 1:length(allSummaries)){
  
  # Get Element
  SummaryElement = allSummaries[[i]]
  
  # Specify the of coefficients to be tested: only health variables
  health = c(16:19)
  
  
  # Test only the joint significance of health variables
  testOutput = try(joint.wald.test(allSummaries[[i]], health, 0.95))
  
  if(class(testOutput) == "try-error"){
   
      
# TODO: reformulate error message function 
    # Display warning and investigate
    msg = paste0("Wald Test failed for Model Element ", i)
    warning(msg)
    
    wald.log[[i]] = "Error"
    
    
  } else{
    
    wald.log[[i]] = testOutput
    
  }
  
  rm(SummaryElement) # clean up
  
}

wald.bound = t(as.data.frame(wald.log))
rownames(wald.bound) = names(allModels)

# Cross check with wald.test from aod package
wald.check = ls()
for (i in 1:length(allModels)){
    wald.test(Sigma = vcov(i), b = coef(i), Terms = 16:19)
    wald.check[[i]] wald.test
    }

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


