#=======================================================================
# This function calculates the area under the curve using the simpsons rule
# y = a time series object containing the y values
# start, end & step should be given in time
# delatax: time interval for area calculation in secs <  sampling interval of the signal 
# end = start + (step * 2 * n), where n is an integer > 0
# f(x) = NA for x < 0, x values are assumed to be always positive
# The function also ouputs a plot indicating the region of calculation
# Ref: Wikipedia:Simpsons Rule
# Date: 06OCT2011
simpsonsarea <- function(y,start,end,step,deltax=deltat(y))
{
#	plot(y,col="black",xlim=c(start,end))

# Converting time values to indices
	start = as.integer(1+abs(start-tsp(y)[1])/deltat(y))
	end = as.integer(1+abs(tsp(y)[1]-end)/deltat(y))
	step = as.integer(step/deltat(y))
	print(c(start,end,step))

# Convert any NA values in y to 0

	y[which(is.na(y)==T)]=0 
	
	n=1
	stop=start
	while (stop <= end-2*step && stop <= length(y)){
		stop = start + step*2*n
		n = n+1
		}
	end=stop
	cat("New end value = ",end,"\n")
	i=start
	area = 0
	while(i < end && (i+2*step) <= length(y)){
		area = area + (y[i]+(y[i+step]*4) +y[i+2*step]) * (step*deltax)/3

# The below two line draws the schematics of the area calculation

#		segments(tsp(y)[1]+deltax*(i-1),y[i],tsp(y)[1]+deltax*(i+2*step-1),y[i+2*step],lty=1,col="red")
#		segments(tsp(y)[1]+deltax*(i-1),0,tsp(y)[1]+deltax*(i-1),y[i+2*step],lty=1,col="lightblue")
		i = i + 2*step
		}
#	area = area
	cat("Area=",area," signal units * sec\n")

	return(area)
}
#a = simpsonsarea(dsf2,1.3,9,0.01)

#==========================================================================