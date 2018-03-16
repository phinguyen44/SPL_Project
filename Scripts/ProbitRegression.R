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


#rm(list= ls())

################################################################################
# SOURCE DATA
wd = paste0(Sys.getenv("HOME"), "/Documents/Projects/SPL_Project")
setwd(wd)

source("Scripts/ReadAndClean.R")
source("Scripts/LoadWald.R")
datasets = read.and.clean(dataset = "easySHARE_rel6_0_0.rda")

#Only keep relevant data sets
df.splits = datasets$df.splits
rm(datasets)

# Load own-built Wald tests
#source("Scripts/LoadWald.R")

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
  testOutput = try(joint.wald.test(allSummaries[[i]], 0.95, health))
  
  if(class(testOutput) == "try-error"){
   
    
    msg = paste0("Wald Test failed for Model Element ", i)
    warning(msg)
    
    wald.log[[i]] = "Error"
    
    
  } else{
    
    wald.log[[i]] = testOutput
    
  }
  
  rm(SummaryElement) # clean up
  
}

wald.bound = as.data.frame(wald.log)
colnames(wald.bound) = names(allModels)


#rm(list= ls()[!(ls() %in% c("allModels", "allSummaries", "wald.bound", "df.splits"))])


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


