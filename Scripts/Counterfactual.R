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
    
    # Create index vector for 50 and 51 year olds
    age_50_51     = ifelse(X$age50 == 1 | X$age51 == 1, 1,0)
    
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

model = allModels$Austria.FEMALE


# Estimate current employment rate for age groups
empl.rate.current       = function(model){
    
    #Initialize storage of results
    empl.rate = numeric(3)
    
    # Select data from model
    X               = model$data
    
    # Predict probability of being employed of all individuals
    empl.probability = predict(object = model, newdata =  X, type = "response")
    
    
  # Create age group Index vectors
    #Initialize result list containing age group index vectors
    IND_row_vec = list()
    
    # Define vector of relevant age groups by youngest age
    group.vec = c(50,55,60)
       
        for (i in group.vec){
            
                    # Age groups of five years
                    k = i + 4
                    
                    # Create labels and contents of each age group
                    vec.label = paste("names.vec.age", i, sep=".") 
                    
                    # Assign ages to the age groups
                    z = assign(vec.label, paste0("age",i:k))
          
          # Assign a 1 to individuals belonging to an age group, 0 otherwise
          IND_row_vec[[i]] = apply(X[, z], 1, sum)
              
        }
    
    # Extract list of relevant age groups, which are each stored in a list 
    IND_row_vec   = IND_row_vec[group.vec]
    

 # Calculate predicted current employment rate for age groups
    
    # loop over age group list
    for (i in 1: length(IND_row_vec)){
        
        empl.rate[i] = empl.probability %*% IND_row_vec[[i]] / sum(IND_row_vec[[i]])
    }
    
    return(empl.rate)
}

empl.current = lapply(allModels, empl.rate.current )


empl.current

# Remark: Should we use a list or numeric values to store results?
# Remark: we use paste0 instead of paste becaue it leaves no space in between
# Should we remove 

