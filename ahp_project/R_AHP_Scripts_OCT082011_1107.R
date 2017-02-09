findpeaks <- function(y,start,end,k,step,h){
	x1 = c()
	s1 = c()
	s2 = c()
	
	j=1
	i = start+k
	l=start
	r = i+k
	while (i <= end-k){
		lm = y[i]-matrix(y[l:(i-1)],nrow=1,ncol=k,byrow=T)
		rm = y[i]-matrix(y[(i+1):r],nrow=1,ncol=k,byrow=T)
		x1[j] = i
		s1[j] = mean(c(max(lm),max(rm)))
		s2[j] = mean(c(mean(lm),mean(rm)))
		i = i+step
		l = i-k
		r = i+k
		j = j+1
	}

#	H = expression(sum( (-density(s)$y)* log10(density(s)$y) ))
#	s=s1
#	Hs1 = eval(H)
	
	x1 = x1[s1>0]
	s1 = s1[s1>0]
	avgs1 = mean(s1)
	sdevs1 = sqrt(var(s1))
	print(c(avgs1,sdevs1)) 
	p2 = c()
	x2 = c()

	i=1
	j=1
	while (i < length(s1)){
		if(s1[i] > 0 && ((s1[i]-avgs1) > (h*sdevs1))){
			p2[j] = y[x1[i]]
			x2[j] = x1[i]
			j= j+1
		}
	i = i+1
	}
	if (length(x2) != length(p2))
	print("Error: x2 and p2 are not of the same length")

	p3 = c()
	x3 = c()

	i=1
	j=1
	while (i < length(x2)){
		pos = i
		delta = x2[i+1]-x2[i]
		if(abs(delta) <= k){
			pos = which(p2[i:i+1]== max(p2[i:i+1]))
			pos = i+(pos-1)
		}
		else{	
			p3[j] = p2[pos]
			x3[j] = x2[pos]
			j = j+1
		}
		i = i+1
		
	}
#	x = x2[order(x2)]
#	p = p[order(x2)]
	print(c(length(x2),length(p2),length(x3),length(p3)))
#	x2 = x2[!is.na(x2)]
#	p = p[!is.na(p)]
	if (length(x3) != length(p3))
	print("Error: x3 and p3 are not of the same length")
return(cbind(x3,p3))
}
#peaks = findpeaks(d10aps,1,31501,70,20,2)










