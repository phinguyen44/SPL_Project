######### Random Notes #############
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


