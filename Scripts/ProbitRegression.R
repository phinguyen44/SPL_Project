yx################################################################################
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












    




################################################################################

#Wald Test goes here

################################################################################


All.marg.effects = lapply(allModels, function(y){
    
    # Initialize vector for results
    marg.effects      = vector("list", 2)
    
    #Calculate average of the sample marginal effects
    pdf.pred            = mean(dnorm(predict(y, type = "link")))
    marg.effects[[1]]   = pdf.pred *coef(y)
    #Comment:remove constant???
    
    
    #Check whether corect   
    #see http://researchrepository.ucd.ie/bitstream/handle/10197/3404/WP11_22.pdf?sequence=1
    #marg. are all very low... why? this seems to be wrong
    
    # Look this up!!!
    #Calculate baseline probabilities of employment
    X                   = y$data
    X_mean              = apply(X, 2, mean)
    beta                = marg.effects[[1]]
    baseline            = X_mean %*% beta
    prob                = pnorm(baseline) 
    marg.effects[[2]]   = prob
    
    return(marg.effects)
    
})








All.marg.effects = lapply(allModels, function(y){
    
    # Initialize vector for results
    marg.effects      = vector("list", 3)
    
    #Calculate average of the sample marginal effects
    pdf.pred            = mean(dnorm(predict(y, type = "link")))
    marg.effects[[1]]   = pdf.pred *coef(y)
    #Comment:remove constant???
    
    #Alternative: Calculate average marginal effects
    X                   = y$data
    # Calculate mean per variable
    X_mean              = apply(X, 2, mean)
    pdf.pred.mean       = dnorm(X_mean %*% coef(y))
    pdf.pred.mean       = rep(pdf.pred.mean, length(coef(y)))
    
    # Calculate pred. value with x + 1
    X_mean.adj          = matrix(rep(X_mean, length(coef(y))), 
                                 nrow = length(coef(y)), byrow = TRUE) + 
        diag(1, length(coef(y)))
    pdf.pred.mean.adj   = dnorm(X_mean.adj %*% coef(y))
    # use dnorm or pnorm?
    pdf.pred.mean.adj   = pdf.pred.mean.adj - pdf.pred.mean
    marg.effects[[2]]   = pdf.pred.mean.adj
    #Check whether corect   
    #see http://researchrepository.ucd.ie/bitstream/handle/10197/3404/WP11_22.pdf?sequence=1
    #marg. are all very low... why? this seems to be wrong
    
    # Look this up!!!
    #Calculate baseline probabilities of employment
    X                   = y$data
    X_mean              = apply(X, 2, mean)
    beta                = marg.effects[[1]]
    baseline            = X_mean %*% beta
    prob                = pnorm(baseline) 
    marg.effects[[3]]   = prob
    
    return(marg.effects)
    
})

#Calculate predicted participation probability of average individual
#Try first for Belgian female



# Check with paper
formattable(data.frame(All.marg.effects))


# Crosscheck with margins package: requires other input format


