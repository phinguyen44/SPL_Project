################################################################################
# SPL_ELP_LoadWald.R
#
################################################################################
# Description:
# Contains a series of Wald tests
#
################################################################################

################################################################################
# Wald Test for Joint Significance

# Note: This a Wald test for the joint signifance of a subset of model
# coefficients
# m = number of restrictions
# Beta is vector of coefficients of size k x 1 
# Confidence.level is the desired confidence level (between 0 and 1). 
# Default: 0.95
# Spec is vector of integers of length 0 < m ≤ k specifying the subset of 
# coefficients to be jointly tested

joint.wald.test = function(model.summary, confidence.level=0.95, spec=NULL) {

    # List all packages needed for session
    neededPackages = c("Matrix")
    allPackages    = c(neededPackages %in% installed.packages()[, "Package"])
    
    # Install packages (if not already installed)
    if (!all(allPackages)) {
        missingIDX = which(allPackages == FALSE)
        needed     = neededPackages[missingIDX]
        lapply(needed, install.packages)
    }
    
    # Load all defined packages
    invisible(lapply(neededPackages, function(x) suppressPackageStartupMessages(
        library(x, character.only = TRUE))))
    
    # Set up test restrictions
    if (is.null(spec)) {
        spec = 1:length(beta) # default joint is significance test
    } else if (is.logical(spec)) {
        stop("spec cannot be a logical vector, tranform to integer vector!")
    } else if (any(spec == 0)) {
        stop("spec cannot contain zero values!")
    } else if (!is.integer(spec)) {
        spec.len = length(spec)
        spec     = as.integer(spec, length = spec.len)
        warning("Converting spec to integer and proceeding")
    }
  
    # Check Input
    if (class(model.summary) != "summary.glm") {
        stop("model.summary must be a glm summary!")
    }
    if (confidence.level > 1 | confidence.level < 0) {
        stop("confidence.level out of bounds!")
    }
    
    # Define test elements
    joint.wald.test        = numeric(4)
    names(joint.wald.test) = c("W","p-value", "df", "Decision")
    beta                   = model.summary$coefficients[,1]
    Var_beta_est           = vcov(model.summary)
    
    # Wald test statistic
    W = t(beta[spec]) %*% solve(Var_beta_est[spec,spec]) %*% beta[spec]
    
    # Set up test output
    pval               = 1-pchisq(W,length(spec))
    joint.wald.test[1] = format(W, digits = 4) 
    joint.wald.test[2] = format(pval, digits = 4)
    joint.wald.test[3] = length(spec)
    joint.wald.test[4] = ifelse(pval <= 1- confidence.level, "Reject H0", 
                                "Cannot reject H0")
    return(joint.wald.test)
}

################################################################################
## Wald Test for Linear Hypothesis

# Note: This a general Wald test for linear hypothesis of model beta 
# coefficients
# m = number of restrictions
# Beta is vector of coefficients of size k x 1 
# Confidence level is the desired confidence level (between 0 and 1). 
# Default: 0.95
# R is Jacobian matrix of size m x k 
# r is restriction function size m x 1
# Example: H0: b1 + b2 = 1 and 2*b1 = 0  (2 restrictions, m = 2)
# R = 1 1 0 0 ...0      and r = 1
#     2 0 0 0 ...0              0 

general.wald.test = function(model.summary, 
                             confidence.level = 0.95, 
                             R                = NULL, 
                             r                = NULL) {
    
    # Check input
    if (class(model.summary) != "summary.glm") {
        stop("model.summary must be a glm summary!")
    }
    if (confidence.level > 1 | confidence.level < 0) {
        stop("confidence.level out of bounds!")
    }
  
    # Define test elements
    general.wald.test        = numeric(4) 
    names(general.wald.test) = c("W","p-value", "df", "Decision")
    beta                     = model.summary$coefficients[, 1]
    Var_beta_est             = vcov(model.summary)
 
    # Set up restriction matrix/vector for linear hypothesis
    # default option is joint significants of all coefficients
    if (is.null(R)) {
        R = diag(1, length(beta)) # default of R is identity matrix 
    } 
    if (is.null(r)) {
        r = rep(0, length(beta)) # default for r is null vector
    } 
    
    # Now R and r definitely exist, check dimensions
    dim_R = dim(R) # Testing R
    k     = length(beta)  # Columns
    m     = length(r) # Rows; general hypothesis test, therefore m = length(r)
    
    ##  Apply test
    if (dim_R[1] != m | dim_R[2] != k) stop("R has wrong dimension!")
    
    # Check rank of R
    if (rankMatrix(R)[1] != m) stop("R has wrong rank!")
    
    # Wald test statistic
    W = t(R%*%beta - r) %*% solve(R%*% Var_beta_est %*% t(R)) %*% (R%*%beta - r)
    
    # Set up test output
    pval                 = 1-pchisq(W,length(r))
    general.wald.test[1] = format(   W, digits = 4) 
    general.wald.test[2] = format(pval, digits = 4)
    general.wald.test[3] = length(r)
    general.wald.test[4] = ifelse(pval <= 1- confidence.level, "Reject H0", "Cannot reject H0")
    
    return(general.wald.test)
}
