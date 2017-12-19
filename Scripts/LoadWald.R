################################################################################
# Wald Test for joint significance

joint.wald.test = function(model.summary, spec, signf.l){
    
    # Define test elements
    joint.wald.test        = numeric(6)
    names(joint.wald.test) = c("Name","W","p-value", "df", "H0" , "Decision")
    beta                   = model.summary$coefficients[,1]
    Var_beta_est           = vcov(model.summary)
    
    # Wald test statistic
    W = t(beta[spec]) %*% solve(Var_beta_est[spec,spec]) %*% beta[spec]
    
    # Set up test output
    chi2               = qchisq(signf.l, df=length(spec))
    pval               = 1-pchisq(W,length(spec))
    joint.wald.test[1] = "Chi2 test"
    joint.wald.test[2] = format(   W, digits = 4) 
    joint.wald.test[3] = format(pval, digits = 4)
    joint.wald.test[4] = length(spec)
    joint.wald.test[5] = "b equal to 0"
    joint.wald.test[6] = ifelse(pval <= 1- signf.l, "Reject H0", "Cannot reject H0")
    joint.wald.test
}

################################################################################
## Wald Test for linear hypothesis

# Note: This a general Wald test for linear hypothesis of model beta coefficients
# m = number of hypothesis
# beta is vector of coefficients
# R is Jacobian matrix of size m x n_beta, r is restriction function size m x 1
# Example: H0: b1 + b2 = 1 and b1 = 0  (2 resitrciotns, m = 2)
# R = 1 1 0 0 ...0      and r = 1
#     1 0 0 0 ...0              0 
#ToDo: incorporate error/warnings if R and r are not specified correctly (i.e. wrong dimensions)


general.wald.test = function(model.summary, signf.l, R = NULL, r = NULL){
    
    # Define test elements
    general.wald.test        = numeric(6)
    names(general.wald.test) = c("Name","W","p-value", "df", "H0" , "Decision")
    beta                     = model.summary$coefficients[, 1]
    Var_beta_est             = vcov(model.summary)
    
    # Set up restriction matrix/vector for linear hypothesis
    # default option is joint significants of all coefficients
    R  = if (is.null(R)){
        R = diag(1, length(beta)) # default of R is identity matrix 
    } else {
        R = R}
    
    r   = if (is.null(r)){
        r = rep(0, length(beta)) # default for r is null vector
    } else {
        r = r}
    
    # Wald test statistic
    W = t(R%*%beta - r) %*% solve(R%*% Var_beta_est %*% t(R)) %*% (R%*%beta - r)
    
    # Set up test output
    chi2                 = qchisq(signf.l, df=length(r))
    pval                 = 1-pchisq(W,length(r))
    general.wald.test[1] = "Chi2 test"
    general.wald.test[2] = format(   W, digits = 4) 
    general.wald.test[3] = format(pval, digits = 4)
    general.wald.test[4] = length(r)
    general.wald.test[5] = "R*b = r"
    general.wald.test[6] = ifelse(pval <= 1- signf.l, "Reject H0", "Cannot reject H0")
    general.wald.test
}


# Test whether it works
B = matrix(0, nrow=2, ncol= 23)
B[1, 4] = 1
B[2, 5] = 2
C = c(0,0)
b = allSummaries$AUT.FEMALE$coefficients[,1]

general.wald.test(allSummaries$AUT.FEMALE, 0.95, B, C)
joint.wald.test(allSummaries$AUT.FEMALE, 4:5 , 0.95)

################################################################################

