rm(list = ls())

# Let's fake a dataset and see if the network evaluates:
set.seed(2020)
N = 50
x = runif(N,-1,1)
e = rnorm(N,0,1)
y = 2*sin(3*pi*x)+e
plot(y~x,pch = 16, col = 'blue')

# Get the data in matrix form:
X = matrix(x,N,1) #cbind(x, x^2, x^3)
Y = matrix(y,N,1)


# Specify activation functions for the hidden and output layers:
sig1 = function(z)
{
   1/(1+exp(-z))
}
sig2 = function(z)
{
   z
}

# Write a function that evaluates the neural network (forward recursion):
# X     - Input matrix (N x p)
# Y     - Output matrix(N x q)
# theta - A parameter vector (all of the parameters)
# m     - Number of nodes on hidden layer
# lam   - Regularisation parameter (see later)
neural_net = function(X,Y, theta,m, lam)
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
   index = max(index) + 1:(m)
   b1    = matrix(theta[index],m,1)
   index = max(index)+1:(q)
   b2    = matrix(theta[index],q,1)
  
   
   # Evaluate network:
   #out   = rep(0,N)
   #error = rep(0,N)
   #for(i in 1:N)
   #{
   #   a0 = matrix(X[i,],p,1)
   #   a1 = sig1(t(W1)%*%a0+b1)
   #   a2 = sig2(t(W2)%*%a1+b2)
   #   out[i]   = a2
   #   error[i] = (a2-Y[i,])^2
   #}
   
   #do this in an exam, not the loop thing
   ones_t = matrix(1,1,N)
   A0 = t(X)
   A1 = sig1(t(W1)%*%A0+b1%*%ones_t)
   A2 = sig2(t(W2)%*%A1+b2%*%ones_t)

   # Calculate error:
   Yhat = t(A2)
   E1 = sum((Y-Yhat)^2)/N
   E2 = 0
   
   # Return predictions and error:
   return(list(out = Yhat, E1 = E1, E2 = E2))
}

# We need to know the number of parameters in the network:

m     = 25 # he wwill give us this dimension in an exam
p     = dim(X)[2]
q     = dim(Y)[2]
npars = p*m+m*q+m+q 
theta_rand = runif(npars,-1,+1)
res = neural_net(X,Y,theta_rand,m,0)
res
obj = function(pars)
{
	 res = neural_net(X,Y,pars,m,0)
   return(res$E1)
}

obj(theta_rand)

res_opt = nlm(obj,theta_rand,iterlim = 500) # interlim is the number of iterations
#score will improve with the more iterations you run
res_opt

res_fitted = neural_net(X,Y,res_opt$estimate,m,0)
res_fitted
plot(y~x,pch = 16,col = 'blue')
#points(res_fitted$out~x,pch = 16, col = 'red')
legend('topright',c('Y','Predictions'), pch = 16, col = c('blue','red'))

xx = seq(min(x),max(x),1/100) # = seq(-1,1, 100)
XX = matrix(xx,length(xx),1) # = matrix(xx, ncol =1)
YY = matrix(0,length(xx),1) # = XX*0
res_response_curve = neural_net(XX,YY,res_opt$estimate,m,0)
lines(res_response_curve$out~xx,lwd = 2)
lines(c(2*sin(3*pi*xx))~xx,col ='grey50')

