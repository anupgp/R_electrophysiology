charge = function(x,delta,tau,steady){

f = delta*(1-exp(-x/tau))+steady

return(f)

}
discharge = function(x,delta,tau,steady){

f = delta*(exp(-x/tau))+steady

return(f)

}

gaussian = function(x,a,b,c){

f = a*exp(-(x-b)^2/2*c^2)

return(f)

}
lognormal = function(x,mean,sd){

f = exp(-(log(x)-mean)^2/2*sd^2)/sd*sqrt(2*pi)

return(f)

}
#=========================================================================
quadratic <- function(x){
return = x^2+2*x+1
}
#=========================================================================

