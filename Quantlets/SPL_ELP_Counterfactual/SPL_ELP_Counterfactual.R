################################################################################
# Counterfactual
#
################################################################################
# Description:
# 
# Calculates current and counterfactual labor participation rates for different # age groups, countries, and genders
# 
################################################################################

################################################################################
# SOURCE DATA
load('easySHARE_clean.RData')

#Only keep relevant data sets
df.splits = datasets$df.splits
rm(datasets)

################################################################################

# Probit for each country and gender 
allModels = lapply(df.splits, function(z) {
    model = glm(z$labor_participationTRUE ~ . -age50, 
                family = binomial(link = "probit"), 
                data   = z)
    return(model)
})

# Return summaries
allSummaries = lapply(allModels, summary)

################################################################################
# Counterfactual Exercise

# Calculate employment rate based on whole population with mean probability

# Estimate current employment rate
empl.rate = function(model) {
    # Select data from model
    X                = model$data
    # Predict probability of being employed of all individuals
    empl.probability = predict(object = model, newdata =  X, type = "response")
    # Calculate predicted current employment rate
    empl.rate        = sum(empl.probability)  / length(empl.probability)
    return(empl.rate)
}

# Function for defining models with modified dataset (perfect health)
X.cf = function(model) {
    # Select data from model
    X                      = model$data
    #Create counterfactual data set with ideal health condition
    X_cf                   = X
    # Find ideal health conditions by finding the minimum value for each 
    # variable
    X_min                  = data.frame(t(apply(X, 2, min)))
    # Replace mean values by values representing no health issues
    names.vec              = c("h_chronic", "h_adlaTRUE", "h_obeseTRUE")
    X_cf[, names.vec]      = X_min[names.vec]
    # Create index vector for 50 and 51 year olds
    age_50_51              = ifelse(X$age50 == 1 | X$age51 == 1, 1,0)
    # Calculate max grip mean of individuals age 50-51 and replace
    X_cf[, c("h_maxgrip")] =  tapply(X$h_maxgrip, age_50_51 == 1, mean)[[2]]
    model$data             = X_cf
    return(model)
}

# Returns a list with all models with modified datasets
allModels.cf = lapply(allModels, X.cf)

# Calculate current and counterfactual employment rates
empl.current     = lapply(allModels, empl.rate)
empl.counterfact = lapply(allModels.cf, empl.rate)

# Store results and display them
employment            = data.frame(cbind(unlist(empl.current), 
                                         unlist(empl.counterfact)))
colnames(employment)  = c("Employment Current" , "Employment Counterf.") 

################################################################################
# Counterfactual exercise for each age group

### Function for current and counterfactual employment rate among age groups
# Default arguments: group size and lower bound of age groups (group.low)

# Estimate current employment rate for age groups
empl.rate.age = function(model, group.size = 5, group.low = c(50,55,60)) {
    
    #Initialize storage of results
    empl.rate        = numeric(3)
    
    # Select data from model
    X                = model$data
    
    # Predict probability of being employed of all individuals
    empl.probability = predict(object = model, newdata =  X, type = "response")
    
    # Create age group Index vectors
    # Initialize result list containing age group index vectors
    IND_row_vec = list()
    
    # Define vector of relevant age groups by youngest age
    for (i in group.low){
        # Define upper bound k of age group
        k         = i + group.size -1
        # Create labels and contents of each age group
        vec.label = paste("names.vec.age", i, sep=".") 
        # Assign ages to the age groups
        z         = assign(vec.label, paste0("age",i:k))
        # For age groups of size 1, use simple indexing (cannot use apply)
        if (length(z) == 1) {
            IND_row_vec[[i]] = X[, z]
        
        # For all age groups greater than size 1      
        } else {
            # Sum over rows to find individuals belonging to age group i
            IND_row_vec[[i]] = apply(X[, z], 1, sum)
        }
    }
    
    # Extract list of relevant age groups, which are each stored in a list 
    IND_row_vec   = IND_row_vec[group.low]
    
# Calculate predicted current employment rate for age groups
# loop over age group list
    for (k in 1: length(IND_row_vec)) {
        empl.rate[k] = empl.probability %*% IND_row_vec[[k]] / 
            sum(IND_row_vec[[k]])
    }
    return(empl.rate)
}

# Calculate current and counterfactual employment rates for age groups
empl.current.age     = t(data.frame(lapply(allModels, empl.rate.age)))
empl.counterfact.age = t(data.frame(lapply(allModels.cf, empl.rate.age)))

employment.age           = data.frame(cbind(empl.current.age, 
                                            empl.counterfact.age))
colnames(employment.age) = paste0(c(rep("empl.current", 3), 
                                    rep( "empl.counterfact", 3)), 
                                  c("50.54","55.59", "60.64"))

# Test for other age groups: Example of employment for each age group
empl.rate.age(allModels$Germany.MALE, group.size = 1, group.low = c(50:63))  
empl.rate.age(allModels.cf$Germany.MALE, group.size = 1, group.low = c(50:63))

################################################################################

# Decline in participation due to decline in health condition
health.decline = function(X) {
    # Differences between counterf. and current participation rates
    dif.60.64 = X[,"empl.counterfact60.64"] - X[, "empl.current60.64"]
    dif.50.54 = X[,"empl.counterfact50.54"] - X[, "empl.current50.54"]
    
    # Absolute difference in current participation rate
    dif.abs   = X[,"empl.current50.54"] - X[,"empl.current60.64"]
    measure   =  (dif.60.64 - dif.50.54) / dif.abs 
    return(measure)
}

participation.health = data.frame(health.decline(employment.age))

################################################################################
# Some Remarks

# Define outout formate of counterfactual estimates: list
empl.each.age = list()

# Calculate employment for each age for all countries
for (i in 1:length(allModels)) {
    k                  = allModels[[i]]
    empl.each.age[[i]] = empl.rate.age(k, group.size = 1, group.low = c(50:64))
}

names(empl.each.age) = names(allModels)
