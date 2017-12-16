################################################################################
# Counterfactual
#
################################################################################
# Description:
# 
# 
################################################################################

################################################################################

rm(list= ls())

################################################################################
# SOURCE DATA

source("Scripts/ReadAndClean.R")
datasets = read.and.clean(dataset = "easySHARE_rel6_0_0.rda")

#Only keep relevant data sets
df.splits = datasets$df.splits
rm(datasets)


################################################################################

# Probit for each country and gender 
allModels = lapply(df.splits, function(z){
    
    z = z[-z$age50] # Multicollinearity
    
    model = glm(z$labor_participationTRUE ~., family = binomial(link = "probit"), data = z)
    
    return(model)
    
})


# Return summaries
allSummaries = lapply(allModels, summary)


# Calculate baseline employment probability

empl.prob = function(model){
    
    # Calculate average person per country & gender 
    X                   = model$data
    X_mean              = data.frame(t(apply(X, 2, mean)))
    
    # Predict probability of being employed of average person
    empl.probability    = predict(object = model, newdata =  X_mean, type = "response")
    return(empl.probability)
}

empl.Models = lapply(allModels, empl.prob)

empl.prob.cf = function(model){
    
    # Calculate average person per country & gender 
    X                   = model$data
    X_mean              = data.frame(t(apply(X, 2, mean)))
   
    # Define no negative health condition
    X_mean_cf           = X_mean
    X_min               = data.frame(t(apply(X, 2, min)))
    
    # Replace mean values by values representing no health issues
    X_mean_cf[15:17]    = X_min[15:17]
    
#TODO: Replace mean maxgrip by maxgrip of age group 50-51
    
    
    # Predict probability of being employed of average person
    empl.probability    = predict(object = model, newdata =  X_mean_cf, type = "response")
    return(empl.probability)
}

empl.cf.Models = lapply(allModels, empl.prob.cf)

employment= data.frame(cbind(empl.Models, empl.cf.Models))


# Why is employment in Switzerland going down???