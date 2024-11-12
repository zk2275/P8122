# golden search function for example 1.2

# log-likehood function of Trinomial distribution function
##theta has to be between 0 and 1

lf = function(x, theta){
  y=x[1]*log(theta+2)+x[2]*log(1-theta) +x[3]*log(theta)
  return(y)
}

# generate some data from true theta = 0.2

n = 100
theta <- 0.2
Xdata <- rmultinom(n, 40, c((2 + theta)/4, (1 - theta)/2,theta/4))

#sample likelihood function

L = function(theta){
  res=sum(apply(Xdata,2,lf, theta=theta))
  return(res)
}

#golden search 

a=0; b=1
w = 0.618
theta0 = a+(b-a)*(1-w)
theta1 = theta0 + (b-a)*(1-w)*w

rlist = NULL
for(i in 1:n){
  
if(L(theta1) >L(theta0)){
  a=theta0;
  theta0 = theta1
  theta1 = theta0 + (b-a)*(1-w)*w
}
  
if(L(theta1) <= L(theta0)){
    b=theta1;
    theta0 = a+(b-a)*(1-w)
    theta1 = theta0 + (b-a)*(1-w)*w
  }  
  
  rlist = rbind(rlist, c(a, b, theta0, theta1, theta1-theta0))
} 