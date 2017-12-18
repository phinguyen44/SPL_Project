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


#NOTE: Current participation rates seem not to be estimated with probit. 
#Rather, those are the rate from sum stats.



# APPROACH 1: Calculate employment rate based on average individual

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
    
    # Find individuals who are 50 years old
    IND_row_50 = apply(X[, 1:14], 1, sum) # 50 year olds have a zero
    age_50_51 = ifelse(IND_row_50==0 | X$age51 == 1, 1,0)
    
    # Calculate max grip mean of individuals age 50-51 and replace
    X_mean_cf[18]       =  tapply(X$h_maxgrip, age_50_51 == 1, mean)[[2]]

    # Predict probability of being employed of average person
    empl.probability    = predict(object = model, newdata =  X_mean_cf, type = "response")
    return(empl.probability)
}

empl.cf.Models = lapply(allModels, empl.prob.cf)

(employment= data.frame(cbind(empl.Models, empl.cf.Models)))


# Why is employment in Switzerland going down -> chronic diseases has positive coefficient



# APPROACH 2: Calculate employment rate based on whole population with naive threshold


empl.prob2 = function(model){
    
    # Calculate average person per country & gender 
    X                   = model$data
    
    # Predict probability of being employed of all individuals
    empl.probability    = predict(object = model, newdata =  X, type = "response")
    empl.ind            = ifelse(empl.probability >=0.5, 1, 0)
    
    # Calculate employment rate
    empl.rate           = sum(empl.ind)  / length(empl.ind)
    return(empl.rate)
}

empl.Models2 = lapply(allModels, empl.prob2)

## Gives very low employment rate



empl.prob.cf2 = function(model){
    
    # Calculate average person per country & gender 
    X                   = model$data
    X_mean              = data.frame(t(apply(X, 2, mean)))
    X_cf                = X
    
    # Define no negative health condition
    X_mean_cf           = X_mean
    X_min               = data.frame(t(apply(X, 2, min)))
    
    # Replace mean values by values representing no health issues
    X_cf[15:17]    = X_min[15:17]
    
    # Find individuals who are 50 years old
    IND_row_50 = apply(X[, 1:14], 1, sum) # 50 year olds have a zero
    age_50_51 = ifelse(IND_row_50==0 | X$age51 == 1, 1,0)
    
    # Calculate max grip mean of individuals age 50-51 and replace
    X_cf[18]       =  tapply(X$h_maxgrip, age_50_51 == 1, mean)[[2]]
    
    # Predict probability of being employed of all individuals
    empl.probability    = predict(object = model, newdata =  X_cf, type = "response")
    empl.ind            = ifelse(empl.probability >=0.5, 1, 0)
    
    # Calculate employment rate
    empl.rate           = sum(empl.ind)  / length(empl.ind)
    return(empl.rate)
}

empl.cf.Models2 = lapply(allModels, empl.prob.cf2)

(employment2= data.frame(cbind(empl.Models2, empl.cf.Models2)))

