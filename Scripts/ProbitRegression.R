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


rm(list= ls()[!(ls() %in% c("df.reg", "df.splits"))])


################################################################################
# LOAD NECESSARY PACKAGES & DATA

# List all packages needed for session
neededPackages = c("aod", "devtools", "margins")
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

################################################################################
#Calculate marginal effects and standard errors


All.marg.effects = lapply(allModels, function(y){
  for (i in 1:length(allModels)){
    x = allModels[[i]]
    pdf = mean(dnorm(predict(x, type = "link")))
    m.e. = pdf*coef(x)
    return(m.e.)
  }
})


# Check with paper
formattable(data.frame(All.marg.effects))

# Crosscheck with margins package: requires other input format

margins

All.marg.effects2 = lapply(allModels, function(y){
    for (i in 1:length(allModels)){
        x = allModels[[i]]
        m.e. = margins(x)
        return(m.e.)
    }
})




################################################################################

#Wald Test goes here

################################################################################

