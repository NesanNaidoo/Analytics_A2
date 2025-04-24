rm(list = ls())

# Let's fake a dataset and see if the network evaluates:
set.seed(2019)
N = 50
x = runif(N,-1,1)
e = rnorm(N,0,sqrt(0.5))
y = 2*sin(3*pi*x)+e
plot(y~x,pch = 16, col = 'blue')

# Get the data in matrix form:
X = matrix(x,N,1) #cbind(x,x^2,x^3)
Y = matrix(y,N,1)

model = function(X,Y,theta)
{
	  w    = theta[1]
	  b    = theta[2]
	  Yhat = X%*%w+b
	  
	  E1   = sum((Yhat-Y)^2)
	  
	  return(list(E1 = E1, Yhat = Yhat))
}

model(X,Y,runif(2,-1,1))

obj = function(pars)
{
	  res_mod = model(X,Y,pars)
	  return(res_mod$E1)
}

res_opt = nlm(obj,runif(2,-1,1))

xx = seq(-1,+1,1/100) #lattice
XX = matrix(xx,ncol = 1)
YY = XX*0 # Just a dummy
res_fitted = model(XX,YY,res_opt$estimate)
lines(res_fitted$Yhat~xx,lwd = 2)




# Specify activation functions for the hidden and output layers:
sig1 = function(z)
{
  tanh(z)
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
   
   #print(theta)
   
   #print(W1)
   #print(W2)
   #print(b1)
   #print(b2)
   
   # Evaluate network:
   #Yhat  = rep(NA,N)
   #error = rep(NA,N)
   #for(i in 1:N)
   #{
   #	  a0 = matrix(X[i,],p,1)
   #	  a1 = sig1(t(W1)%*%a0+b1)
   #	  a2 = sig2(t(W2)%*%a1+b2)
   #	  Yhat[i] = a2
   #	  error[i] = (Yhat[i]-Y[i,])^2
   #}
   ones = matrix(1,1,N)
   A0   = t(X)
   A1   = sig1(t(W1)%*%A0+b1%*%ones)
   A2   = sig2(t(W2)%*%A1+b2%*%ones)
   Yhat = t(A2)
   
   # Calculate error:
   
   E1 = sum((Y - Yhat)^2)
   E2 = E1 + nu/N*(sum(W1^2)+sum(W2^2)) #modified objective/penalised function
   
   # Return predictions and error:
   return(list(Yhat = Yhat, E1 = E1, E2 = E2))
}

# We need to know the number of parameters in the network:

m     = 10
p     = dim(X)[2]
q     = dim(Y)[2]
npars = p*m+m*q+m+q
npars
theta_rand = runif(npars,-1,1)
res = neural_net(X,Y,theta_rand,m,0)
res


obj = function(pars)
{
	res_model = neural_net(X,Y,pars,m,0)
	return(res_model$E1)
}


obj(theta_rand)

res_opt = nlm(obj,theta_rand,iterlim = 1000)
res_opt

res_fitted = neural_net(X,Y,res_opt$estimate,m,0)
res_fitted
plot(y~x,pch = 16,col = 'blue')
points(res_fitted$Yhat~x,pch = 16, col = 'red')
legend('topright',c('Y Observed','Predictions'), pch = 16, col = c('blue','red'))

# A response curve
xx = seq(-1,1,1/100)
XX = matrix(xx,ncol = 1)
YY = XX*0
res_response_curve = neural_net(XX,YY,res_opt$estimate,m,0)
lines(res_response_curve$Yhat~xx,lwd = 2,col = 'red')
lines(c(2*sin(3*pi*xx))~xx,col ='grey50')

##################################################################
# Validation analysis
##################################################################

#set.seed(4) different seeds produce diff curves
N = dim(X)[1]
set = sample(1:N, 0.8*N, replace = FALSE) # replace becasue we dont waant the same to repeat in the observation
X_train = matrix(X[set,], ncol = 1)
Y_train = matrix(Y[set,], ncol = 1)
X_val = matrix(X[-set,], ncol = 1)
Y_val = matrix(Y[-set,], ncol = 1)
dim(X_train)
dim(X_val)

nu = 1
obj_pen = function(pars)
{
  res_model = neural_net(X_train, Y_train, pars, m, nu) #NB training data
  return(res_model$E2)
}

theta_rand = runif(npars, -1, 1)
obj(theta_rand)

res_opt = nlm(obj_pen,theta_rand,iterlim = 1000)
res_opt

res_fitted = neural_net(X,Y,res_opt$estimate,m,0)
res_fitted

plot(y~x,pch = 16,col = 'blue')
points(Y_val~X_val, pch = 16, col = 'hotpink', cex= 2)
points(res_fitted$Yhat~x,pch = 16, col = 'red')
legend('topright',c('Y Observed','Predictions'), pch = 16, col = c('blue','red'))

# A response curve
xx = seq(-1,1,1/100)
XX = matrix(xx,ncol = 1)
YY = XX*0
res_response_curve = neural_net(XX,YY,res_opt$estimate,m,0)
lines(res_response_curve$Yhat~xx,lwd = 2,col = 'red')
lines(c(2*sin(3*pi*xx))~xx,col ='grey50')


n_nu = 10
Val_error = rep(NA, n_nu)
nu_seq = exp(seq(-6, 1, length = n_nu))
for (i in 1:n_nu)
{
  nu = nu_seq[i]
  res_opt = nlm(obj_pen, theta_rand, iterlim = 1000)
  
  res_val = neural_net(X_val, Y_val, res_opt$estimate, m, 0)
  Val_error[i] = res_val$E1
  
  print(paste0('nu = ', round(nu, 4)))





# A response curve
xx = seq(-1,1,1/100)
XX = matrix(xx,ncol = 1)
YY = XX*0
res_response_curve = neural_net(XX,YY,res_opt$estimate,m,0)
lines(res_response_curve$Yhat~xx,lwd = 2,col = 'red')
lines(c(2*sin(3*pi*xx))~xx,col ='grey50')

plot(res_opt$estimate, type = "h", ylim = c(-1, 1)*10)
plot(res_opt$gradient, type = "h", ylim = c(-1, 1)*0.001)
}