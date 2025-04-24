rm(list = ls())

# Let's fake a dataset and see if the network evaluates:
set.seed(2019)
N = 50
x = runif(N,-1,1)
e = rnorm(N,0,sqrt(0.5))
y = 2*x+2*sin(1.5*pi*x)+e
plot(y~x,pch = 16, col = 'blue')

# Get the data in matrix form:
X = matrix(x,N,1) #cbind(x,x^2,x^3)
Y = matrix(y,N,1)


# Specify activation functions for the hidden and output layers:
sig1 = function(z)
{
  tanh(z)#1/(1+exp(-z))
}
sig1. = function(z)
{
	1 - tanh(z)^2
}
sig2 = function(z)
{
   z
}
sig2. = function(z)
{
   1+0*z
}

g = function(Y,aL)
{
	 0.5*(Y-aL)^2
}


g. = function(Y,aL)
{
	 -1*(Y-aL)
}
# Write a function that evaluates the neural network (forward recursion):
# X     - Input matrix (N x p)
# Y     - Output matrix(N x q)
# theta - A parameter vector (all of the parameters)
# m     - Number of nodes on hidden layer
# lam   - Regularisation parameter (see later)
neural_net = function(X,Y, theta,m, nu)
{
	 # Relevant dimensional variables:
   N = dim(X)[1]
   p = dim(X)[2]
   q = dim(Y)[2]
   
   # Populate weight-matrix and bias vectors:
   index = 1:(p*m)
   W1    = matrix(theta[index],p,m)
   index = max(index)+1:(m*q)
   W2    = matrix(theta[index],m,q)
   index = max(index)+1:m
   b1    = matrix(theta[index],m,1)
   index = max(index)+1:q
   b2    = matrix(theta[index],q,1)
  
   dW1   = 0*W1 # Creates storage
   dW2   = 0*W2
   db1   = 0*b1   
   db2   = 0*b2
   
   # Evaluate network:
   Yhat  = rep(NA,N)
   error = rep(NA,N)
   for(i in 1:N)
   {
   	  a0 = matrix(X[i,],p,1)
   	  z1 = t(W1)%*%a0+b1
   	  a1 = sig1(z1)
   	  z2 = t(W2)%*%a1+b2
   	  a2 = sig2(z2)
   	  
   	  
   	  Yhat[i]  = a2
   	  error[i] = g(Y[i],Yhat[i])
   	  
   	  d2 = g.(Y[i],Yhat[i])*sig2.(z2)
   	  d1 = (W2%*%d2)*sig1.(z1)
   	  
   	  db1 = db1 + d1
   	  db2 = db2 + d2
   	  dW1 = dW1 + (a0%*%t(d1))
   	  dW2 = dW2 + (a1%*%t(d2)) 
   	  
   	}
   
   dW1 = dW1 + nu*2*W1
   dW2 = dW2 + nu*2*W2
   # Calculate error:
   
   E1 = sum(error)
   E2 = E1+nu*(sum(W1^2)+sum(W2^2)) # modified objective/penalised objective
   
   # Return predictions and error:
   return(list(Yhat = Yhat,grad =c(dW1,dW2,db1,db2)/N , E1 = E1/N, E2 = E2/N))
}

# We need to know the number of parameters in the network:

m     =  15
p     = dim(X)[2]
q     = dim(Y)[2]
npars = p*m+m*q+m+q
npars
theta_rand = runif(npars,-1,1)
nu  = 0.5
res = neural_net(X,Y,theta_rand,m,nu)
res

# Response curve:
xx = seq(-1,+1,1/100) #lattice
XX = matrix(xx,ncol = 1)
YY = XX*0 # Just a dummy
res_fitted = neural_net(XX,YY,theta_rand,m,nu)
lines(res_fitted$Yhat~xx,lwd  = 2,col = 'red')

#=============================================
# Gradient Descent
#=============================================

grad_descent = function(theta_start,iterations,h,plt = TRUE)
{
	theta = theta_start
	pars  = matrix(NA,length(theta),iterations)
	pars[,1] =  theta
 
	error = rep(NA,iterations)
	ss    =  sample(1:dim(X)[1],1*dim(X)[1],replace = FALSE)
	Xsmp  = matrix(X[ss,],ncol = 1)
  Ysmp  = matrix(Y[ss,],ncol = 1)
  res   = neural_net(Xsmp,Ysmp,theta,m,nu)
	error[1] = res$E2
	for(i in 2:iterations)
	{
		grad_norm = res$grad/sqrt(sum(res$grad^2)) # Can use to fix step size. Will oscillate at minimum.
		theta = theta - h*res$grad
		ss    =  sample(1:dim(X)[1],1*dim(X)[1],replace = FALSE)
	  Xsmp  = matrix(X[ss,],ncol = 1)
    Ysmp  = matrix(Y[ss,],ncol = 1)
    res   = neural_net(Xsmp,Ysmp,theta,m,nu)
	  error[i] = res$E2
	  pars[,i] = theta
	  
	  if(plt)
	  {
	  	if(i%%100==0)
	  	{
	  		par(mfrow = c(2,2))
	  		plot(error,type = 'l',main = paste0('Iteration =',i))

        plot(res$grad,type = 'h',ylim = c(-1,1)*0.1)
        abline(h = c(-1,1)*0.01,lty = 2)

        plot(y~x,pch = 16, col = 'blue')
        res_fitted = neural_net(XX,YY,theta,m,nu)
        lines(res_fitted$Yhat~xx,lwd  = 2,col = 'red')
	  	}
	  }
	  
	  
	}
	return(list(error = error,theta_hat = theta,grad_hat = res$grad,pars = pars))
}
quartz()

nu =    0.0
res_opt = grad_descent(theta_rand,10000,1,TRUE)

par(mfrow = c(2,2))
plot(res_opt$error[-c(1:1000)],type = 'l')

plot(res_opt$grad_hat,type = 'h',ylim = c(-1,1)*0.1)
abline(h = c(-1,1)*0.01,lty = 2)

#plot(res_opt$pars[1,],type = 'n',ylim = range(res_opt$pars))
#for(j in 1:npars)
#{
#	lines(res_opt$pars[j,])
#}



plot(y~x,pch = 16, col = 'blue')
res_fitted = neural_net(XX,YY,res_opt$theta_hat,m,nu)
lines(res_fitted$Yhat~xx,lwd  = 2,col = 'red')

