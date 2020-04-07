#PS_8
#Question4
set.seed(100)

N<-100000
k<-10

x<-matrix(rnorm(N*k,mean=0,sd=0.5), N,k)
x[,1]<-1

eps<-rnorm(N,mean=0, sd=0.5)
beta<- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2 )
y<-x%*%beta+eps

#Question 5
beta_hat<-solve(t(x)%*%x)%*%(t(x)%*%y)
print(beta_hat)

#Question6 
#step size
alpha<-0.0000003

objfun <- function(beta,y,x) {
  return (sum((y-x%*%beta)^2))
}

gradient <- function(beta,y,x) {
  return ( as.vector(-2*t(x)%*%(y-x%*%beta)) ) 
}
beta<- runif(dim(x)[2])

iter  <- 1
beta0 <- 0*beta_hat
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,y,x)
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

print(iter)
print(paste("The minimum of f(beta,y,x) is ", beta, sep = ""))


#Question7 
library(nloptr)

objfun <- function(beta,y,x) {
  return (sum((y-x%*%beta)^2))
}

beta0 <- runif(dim(x)[2]) 
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,x=x)
LBFGS.result <- result$solution
print(LBFGS.result)
#Nelder_Mead
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3)
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,x=x)
NM.result <- result$solution
print(LBFGS.result)

#Question 8
library(nloptr)

objfun  <- function(theta,y,X) {
  
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-x%*%beta)/sig)^2) ) 
  return (loglike)
}

gradient <- function (theta ,y,x) {
  grad <- as.vector (rep (0, length (theta )))
  beta <- theta [1:( length ( theta) -1)]
  sig <- theta [ length (theta )]
  grad [1:( length ( theta) -1)] <- -t(x)%*%(y - x%*%beta)/(sig ^2)
  grad[ length (theta )] <- dim(x)[1]/sig - crossprod (y-x%*%beta)/(sig^3)
  return ( grad )                                                 
}


beta0 <- runif(dim(X)[2]+1)
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)

result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,x=x)
Beta_hat_MLElbfg <- result$solution[1:(length(result$solution)-1)]
print(BETA_HAT_MLElbfg)


#Question 9
library(stargazer)
est<- lm(y ~ x -1)
summary(est)
betalm <- est$coefficients

stargazer(est)

cbind(beta,beta_hat,LBFGS.result,NM.result, Beta_hat_MLElbfg,betalm)


