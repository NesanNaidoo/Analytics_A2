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
sig1. = function(z) # sig. means prime
{
	
}
sig2 = function(z)
{
   z
}
sig2. = function(z)
{
  1+0*z # so that it converts it into a vector or a matrix form instead of just a scaler
}

g = function(Y,aL)
{
	 0.5*(Y-aL)^2 # coded obj fn as an obj fn but with neural etworks, usually integrate within, use 0.5 so that hen taking the derivative the 2 comes forward and the .5 dissapears
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
  
   dW1 = 0*W1 # creates storage
   dW2 =  0*W2
   db1 = 0*b1
   db2 = 0*b2
   
   # Evaluate network:
   Yhat  = rep(NA,N)
   error = rep(NA,N)
   for(i in 1:N) # loop through all observations
   {
   	  a0 = matrix(X[i,],p,1) # boundary cond
   	  z1 = t(W1)%*%a0+b1
   	  a1 = sig1(z1) # translation
   	  z2 = t(W2)%*%a1+b2
   	  a2 = sig2(z2) # output translations
   	  
   	  
   	  Yhat[i]  = a2
   	  error[i] = g(Y[i],Yhat[i]) # g is the objective function
   	  
   	  d2 = g.(Y[i], Yhat[i])*sig2.(z2) # working gradients of the neural network
   	  d1 = (W2%*%d2)*sig1.(z1)
   	  
   	  db1 = db1 + d1 # get the cummulative of the gradient because each ci is one loop, 
   	  db2 = db2 + d2 # so each time the loop runs, you ant to accumulate the gradients to get c = sum of ci
   	  dW1 = dW1 + (a0%*%t(d1)) # if the obj was not C but i/n*C then you would needto divide each of the derivatives 
   	  dW2 = dW2 + (a1%*%t(d2)) # by 1/n
   	}
   
   
   dW1 = dW1 + nu*2*W1
   dW2 = dW2 + nu*2*W2
   
   # Calculate error:
   
   E1 = sum(error)
   E2 = E1 + nu/Nsum(W1^2) + sum(2^2)# modified objective/penalised objective; regularisation is after E1 +...
   
   # Return predictions and error:
   return(list(Yhat = Yhat,grad =c(dW1, dW2, db1, db2) , E1 = E1, E2 = E2))
}

# We need to know the number of parameters in the network:

m     =  15
p     = dim(X)[2]
q     = dim(Y)[2]
npars = p*m+m*q+m+q
npars
theta_rand = runif(npars,-1,1)
res = neural_net(X,Y,theta_rand,m,0)
res

# Response curve:
xx = seq(-1,+1,1/100) #lattice
XX = matrix(xx,ncol = 1)
YY = XX*0 # Just a dummy
res_fitted = neural_net(XX,YY,theta_rand,m,0)
lines(res_fitted$Yhat~xx,lwd  = 2,col = 'red')

#=============================================
# Gradient Descent
#=============================================

grad_descent = function(theta_start,iterations,h,plt = TRUE)
{
  
  theta = theta_start
  pars = matrix(x(NA, length(theta), iterations))
  pars[,1] = theta_start
  
  error = rep(NA, iterations)
  res = neural_net(X,Y,theta,m,0)
  error[1] = res$E1
  
	for(i in 2:iterations)
	{
	  theta = theta - h*res$grad
	  res = neural_net(X,Y,theta,m,0)
	  error[1] = res$E1
	  pars[,i] = theta
	}
	return(list(error = error, theta_hat = theta, grad_hat = res$grad, pars = pars))
}
#quartz() - mac; window() - for windows
nu = 0.1
res_opt = grad_descent(theta_rand, 10000, 0.01, TRUE) # hasnt reached the min at 5000 iterations, increase learning rate if it takes too long

par(mfrow = c(2,2))
plot(res_opt$error, type = 'l') # x axis tells you the iteration number

plot(res_opt$grad_hat, type = 'h', ylim = c(-1,1)*0.1) # x axis tells ou what the parameter is
abline(h = c(-1,1)*0.01, lty = 2)

plot(res_opt$pars[1,], type = 'n', ylim = range(res_opt$pars))
for (j in 1:npars) 
{
  lines(res_opt$pars[j,])
}

# effect of shrinkage, num of parameters decreased