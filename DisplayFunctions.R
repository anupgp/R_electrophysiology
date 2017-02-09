#========================================================================
# Displays all columns of a matrix/timeseries obj as individual traces
# equally distributes the avilable grey levels to the set of n traces

plotahp <- function(d){
	n = dim(d)[2]
	greylevels = grey(seq(from=1,to=0,by=-(1/(n+1)))[1:n+2])
	plot(d[,1],xlim=c(1.25,7),col= greylevels[1])
	i=1+1
	while(i<=n){
		lines(d[,i],col=greylevels[i])
		i=i+1
	}
return()
}
#=========================================================================
