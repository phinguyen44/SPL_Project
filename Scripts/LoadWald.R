################################################################################
# Wald Test for joint significance

# Note: This a Wald test for the joint signifance of a subset of model coefficients
# m = number of restrictions
# Beta is vector of coefficients of size k x 1 
# Signf.level is the desired significance level (between 0 and 1). Default: 0.95
# Spec is vector of integers of length 0 < m ≤ k specifying the subset of coefficients
# to be jointly tested

library("Matrix")

joint.wald.test = function(model.summary, signf.level = 0.95, spec = NULL){
  
  # Set up test restrictions
  if(is.null(spec)){
    spec = 1:length(beta) # default joint is significance test
  } else if(is.logical(spec)){ stop("spec cannot be a logical vector, tranform to interger vector!")
  } else if(any(spec == 0)){ stop("spec cannot contain zero values!")
  } else if(!is.integer(spec)){
      spec.len = length(spec)
      spec = as.integer(spec, length = spec.len)
      warning("Converting spec to integer and proceeding")
    }
  
    # Check Input
    if(class(model.summary) != "summary.glm") stop("model.summary must be a glm summary!")
    if(signf.level > 1 | signf.level < 0) stop("signf.level out of bounds!")
    
    # Define test elements
    joint.wald.test        = numeric(6)
    names(joint.wald.test) = c("Test","W","p-value", "df", "H0" , "Decision")
    beta                   = model.summary$coefficients[,1]
    Var_beta_est           = vcov(model.summary)
    
    # Wald test statistic
    W = t(beta[spec]) %*% solve(Var_beta_est[spec,spec]) %*% beta[spec]
    
    # Set up test output
    chi2               = qchisq(signf.level, df = length(spec))
    pval               = 1-pchisq(W,length(spec))
    joint.wald.test[1] = "Wald"
    joint.wald.test[2] = format(   W, digits = 4) 
    joint.wald.test[3] = format(pval, digits = 4)
    joint.wald.test[4] = length(spec)
    joint.wald.test[5] = "b equal to 0"
    joint.wald.test[6] = ifelse(pval <= 1- signf.level, "Reject H0", "Cannot reject H0")
    joint.wald.test
}

################################################################################
## Wald Test for linear hypothesis

# Note: This a general Wald test for linear hypothesis of model beta coefficients
# m = number of restrictions
# Beta is vector of coefficients of size k x 1 
# Signf.level is the desired significance level (between 0 and 1). Default: 0.95
# R is Jacobian matrix of size m x k 
# r is restriction function size m x 1
# Example: H0: b1 + b2 = 1 and 2*b1 = 0  (2 restrictions, m = 2)
# R = 1 1 0 0 ...0      and r = 1
#     2 0 0 0 ...0              0 

# For Debug
# model.summary = allSummaries[[1]] 

general.wald.test = function(model.summary, signf.level = 0.95, R = NULL, r = NULL){
    
    # Check input
    if(class(model.summary) != "summary.glm") stop("model.summary must be a glm summary!")
    if(signf.level > 1 | signf.level < 0) stop("signf.level out of bounds!")
  
    # Define test elements
    general.wald.test        = numeric(6) 
    names(general.wald.test) = c("Test","W","p-value", "df", "H0" , "Decision")
    beta                     = model.summary$coefficients[, 1]
    Var_beta_est             = vcov(model.summary)
 
    # Set up restriction matrix/vector for linear hypothesis
    # default option is joint significants of all coefficients
    if (is.null(R)){
        R = diag(1, length(beta)) # default of R is identity matrix 
    } 
    
    if (is.null(r)){
        r = rep(0, length(beta)) # default for r is null vector
    } 
    
    # Now R and rdefinitely exist, check dimensions
    # R must be a matrix/dataframe/vector of size m x k, where m is the number of restrictions and k the number of all coefficients in the model summary.
    
    ## Testing R
    dim_R = dim(R)
    
    # Columns
    k = length(beta) # nrow(model.summary$coefficients)
    
    # Rows; general hypothesis testing, therefore m = length(r)
    m = length(r)
    
    ##  Apply test
    if(dim_R[1] != m | dim_R[2] != k) stop("R has wrong dimension!")
    
    # Check rank of R
    if(rankMatrix(R)[1] != m) stop("R has wrong rank!")
    
    # Wald test statistic
    W = t(R%*%beta - r) %*% solve(R%*% Var_beta_est %*% t(R)) %*% (R%*%beta - r)
    
    # Set up test output
    chi2                 = qchisq(signf.level, df=length(r))
    pval                 = 1-pchisq(W,length(r))
    general.wald.test[1] = "Wald"
    general.wald.test[2] = format(   W, digits = 4) 
    general.wald.test[3] = format(pval, digits = 4)
    general.wald.test[4] = length(r)
    general.wald.test[5] = "R*b = r"
    general.wald.test[6] = ifelse(pval <= 1- signf.level, "Reject H0", "Cannot reject H0")
    general.wald.test
}
################################################################################

