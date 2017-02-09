#This Function will serve as a low-pass guassian filter for time series data
#Author Anup Pillai
#Date SEP092011
 gkernal <- function(radius)
{
#Filter function
 width <- round(radius)
 gkernal <- rep(0,width*2+1)
 sigma <- radius/3
 norm <- 1/(sqrt(2*pi)*sigma)
 coeff <- 2*sigma*sigma
 x <- seq(from=-width,to=width,by=1)
 gaussian <- c(expression(norm*exp(-x*x/coeff)))
 kernal <- eval(gaussian)
 kernal <- kernal/sum(kernal)
 return(kernal)
}