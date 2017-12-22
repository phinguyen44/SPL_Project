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
    
    
    model = glm(z$labor_participationTRUE ~ . -age50, family = binomial(link = "probit"), data = z)
    
    return(model)
    
})


# Return summaries
allSummaries = lapply(allModels, summary)

#TODO: Use source file for model estimation

################################################################################
# Counterfactual Exercise

# Calculate employment rate based on whole population with mean probability

# Estimate current employment rate
empl.rate.current       = function(model){
    
    # Select data from model
     X               = model$data
    
    # Predict probability of being employed of all individuals
    empl.probability = predict(object = model, newdata =  X, type = "response")

    # Calculate predicted current employment rate
    empl.rate        = sum(empl.probability)  / length(empl.probability)
    
    return(empl.rate)
}

empl.current = lapply(allModels, empl.rate.current )

empl.rate.counterfact   = function(model){
    
    # Select data from model
    X                   = model$data
    
    #Create counterfactual data set with ideal health condition
    X_cf                = X
    
    # Find ideal health conditions by finding the minimum value for each variable
    X_min               = data.frame(t(apply(X, 2, min)))
    
    # Replace mean values by values representing no health issues
     names.vec          = c("h_chronic", "h_adlaTRUE", "h_obeseTRUE")
     X_cf[, names.vec]  = X_min[names.vec]
    
     
    # TODO: Alternatively: define models differently, such that age50 is made explicit
    # Find individuals who are 50 years old
    names.vec.age = c("age51", "age52", "age53", "age54", "age55", "age56", "age57", "age58", 
                   "age59", "age60", "age61", "age62", "age63", "age64")
    IND_row_50    = apply(X[, names.vec.age], 1, sum) # 50 year olds have a zero
    
    # Create index vector for 50 and 51 year olds
    age_50_51     = ifelse(IND_row_50 == 0 | X$age51 == 1, 1,0)
    
    # Calculate max grip mean of individuals age 50-51 and replace
    X_cf[, c("h_maxgrip")] =  tapply(X$h_maxgrip, age_50_51 == 1, mean)[[2]]

    # Predict probability of being employed of all individuals
    empl.probability    = predict(object = model, newdata =  X_cf, type = "response")
   
    # Calculate predicted counterfactual employment rate
    empl.rate           = sum(empl.probability)  / length(empl.probability)
    return(empl.rate)
}

empl.counterfact = lapply(allModels, empl.rate.counterfact)

(employment= data.frame(cbind(empl.current, empl.counterfact)))

# Note: the expected value of probability converges to true population mean

################################################################################
# Counterfactual exercise for each age group

dat = allModels$Austria.FEMALE$data
summary(dat)


# Estimate current employment rate for age groups
empl.rate.current       = function(model){
    
    empl.rate = numeric(3)
    
    # Select data from model
    X               = model$data
    
    # Predict probability of being employed of all individuals
    empl.probability = predict(object = model, newdata =  X, type = "response")
    
    names.vec.age.54 = c("age50", "age51", "age52", "age53", "age54") 
    names.vec.age.59 = c("age55", "age56", "age57", "age58", "age59") 
    names.vec.age.64 = c("age60", "age61", "age62", "age63", "age64")
 
    # Define index vectors for age groups
    IND_row_54    = apply(X[, names.vec.age.54], 1, sum)
    IND_row_59    = apply(X[, names.vec.age.59], 1, sum)
    IND_row_64    = apply(X[, names.vec.age.64], 1, sum)
    
    
    # Calculate predicted current employment rate for age groups
    empl.rate[1]        = sum(empl.probability)  / length(empl.probability)
    empl.rate[2] 
    empl.rate[3] 
    
    return(empl.rate)
}

empl.current = lapply(allModels, empl.rate.current )






