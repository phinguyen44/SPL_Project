probitMLE = function(params, X, y){
  b = params
  mu = X%*%b
  ll = sum(y*pnorm(mu, log.p=T) + (1-y)*pnorm(-mu, log.p=T))
  ll
}

X = as.matrix(df.splits$AUT.FEMALE)
y = X[,24]
X = X[,1:23]
### Fit with optim
outMLE = optim(1, probitMLE, X=X, y=y, 
               control=list(fnscale=-1, maxit=1000, reltol=1e-8))  

X= X[,20:22]
K = ncol(X)

vi <- lm(y ~ age51 + age52 + age53)$coefficients
# negative log-likelihood
probit.nll <- function (beta) {
  exb <- exp(X%*%beta)
  prob<- rnorm(exb)
  logexb <- log(prob)
  y0 <- (1-y)
  logexb0 <- log(1-prob)
  yt <- t(y)
  y0t <- t(y0)
  -sum(yt%*%logexb + y0t%*%logexb0)
}

# gradient
probit.gr <- function (beta) {
  grad <- numeric(K)
  exb <- exp(X%*%beta)
  prob <- rnorm(exb)
  for (k in 1:K) grad[k] <- sum(X[,k]*(y - prob))
  return(-grad)
}

# direct minimization
fit <- optim(c(1:3), probit.nll, gr = probit.gr, method = "BFGS", hessian =  TRUE)
