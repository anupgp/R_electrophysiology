# Date: OCT102011 Time: 2000 Author: Anup G. Pillai
# Find the maximas and minimas of the given vector v
# Ref: The algorithm was obtained from the webpage of Eli Billauer (http://billauer.co.il/peakdet.html

peaks = function(v,delta){
	mxv = c()
	mxx = c()
	mnv = c()
	mnx = c()
	x = 1: length(v)-1
	i=1
	lookformax = 1
	mn = +Inf; mnpos = NA
	mx = -Inf; mxpos = NA
	while(i<length(v)){
		this = v[i]		
		if(this > mx ) { mx = this; mxpos = x[i]} 
		if(this < mn ) { mn = this; mnpos = x[i]}
		
		if(lookformax){
			if(this < mx-delta){
				mxv = append(mxv,mx,after=length(mxv))
				mxx = append(mxx,mxpos,after= length(mxx))
				mn = this; mnpos = x[i]
				lookformax = 0
			}
		}
		else{
			if(this > mn+delta){
				mnv = append(mnv,mn,after = length(mnv))
				mnx = append(mnx,mnpos,after= length(mnv))
				mx = this; mxpos = x[i]
				lookformax = 1
			}
		}
		i = i+1
	}
	if (is.null(mxx)) {mxx = c(0); mxv = c(0); mnx = c(0); mnv = c(0)}
	less = length(mxx)-length(mnx)
	print(less)
	if(less > 0){mnx=append(mnx,rep(0,less),after = length(mnx));mnv=append(mnv,rep(0,less),after=length(mnv))}
	names(mxx)=NULL; names(mxv)=NULL
	if(is.ts(v)){
#		mxx = mxx*deltat(v)+ start(v)[1]* as.integer(mxx[length(mxx)]>0)
#		mnx = mnx*deltat(v)+ start(v)[1]* as.integer(mnx[length(mnx)]>0)
		mxx = mxx*deltat(v)
		mnx = mnx*deltat(v)
	}

	print(c(length(mxx),length(mxv),length(mnx),length(mnv)))
	print(mxx);print(mxv);print(mnx);print(mnv)
	
return(cbind(mxx,mxv,mnx,mnv))
}

#============================================================================



						
