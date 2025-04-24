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

model = function(X,Y, theta){
  w =theta[1]
  b = theta[2]
  Yhat = X %*% w+b
  
  E1 = sum((Yhat-Y)^2)
  
  return(list(E1 = E1, Yhat = Yhat))
}

#fixed globally so that the data doesnt change
obj = function(pars) #essentially the parameters of the function
  {
  res_mod = model(X,Y, pars)
  return(res_mod$E1)
}

# we can use nlm in exams

res_opt = nlm(obj, runif(2,-1,1))

#response curve - how does our model respond to different options in the input, for an interval, pick a sequence and ask the model to predict the response and connect that to get the respponse curve

xx = seq(-1, 1, 1/100) #lattice
XX = matrix(xx, ncol = 1)
YY = XX*0 #dummy
res_fitted = model(XX, YY, res_opt$estimate)
lines(res_fitted$Yhat~xx, lwd = 2)

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
  # first 1 to pxm paramters , inout node to the 1st layeer
   index = 1:(p*m)
   W1 = matrix(theta[index], p, m)
   index = max(index)+1:(m*q)# conect from 1st layer with m nodes to the output layer with q nodes
   W2 = matrix(theta[index], m, q)
   index = max(index) + 1:m
   b1 = matrix(theta[index], m, 1)
   index = max(index)+1:q
   b2    = matrix(theta[index],q,1)
   
   print(theta)
   print(W1)
   print(W2)
   print(b1)
   print(b2)
   
   # Evaluate network:
   Yhat = rep(NA, N)
   error = rep(NA, N) #for every prediction you need an error
   for(i in 1:N){
     a0 = matrix(X[i,],p,1)
     a1 = sig1(t(W1)%*%a0+b1)
     a2 = sig2(t(W2)%*%a1+b2)
     Yhat[i] = a2
     error[i] = (Yhat[i]-Y[i,])^2
   }
   
   

   # Calculate error:
   
   E1 = sum(error)
   E2 = 0
   
   # Return predictions and error:
   return(list(Yhat = Yhat, E1 = E1, E2 = E2))
}

# We need to know the number of parameters in the network:

m     = 10
p     = dim(X)[2]
q     = dim(Y)[2]
npars = p*m+m*q+m+q 
npars
theta_rand = runif(npars,-1,+1)
res = neural_net(X,Y,theta_rand,m,0) # takes the first 10 param
res



obj = function(pars){
  res_model = neural_net(X,Y, pars, m, 0)
  return(res_model$E1)
}
obj(theta_rand)

res_opt = nlm(obj,theta_rand,iterlim = 500)
res_opt

res_fitted = neural_net(X,Y,res_opt$estimate,m,0)
res_fitted
plot(y~x,pch = 16,col = 'blue')
#points(res_fitted$out~x,pch = 16, col = 'red')
legend('topright',c('Y Observed','Predictions'), pch = 16, col = c('blue','red'))

# A response curve
xx = 
XX = 
YY = 
res_response_curve = neural_net(XX,YY,,m,0)
lines(res_response_curve$out~xx,lwd = 2)
lines(c(2*sin(3*pi*xx))~xx,col ='grey50')

