sub(pattern="[.abf]{4}",x="AUG092011_C3_CV.abf","")
sub(pattern="[_]{1}[CV]{2}[.abf]{4}",x="AUG092011_C3_CV.abf","")
gsub(pattern="([A-Z0-9]{9})([_]{1})([C1-9]{2})([_]{1})",x="AUG092011_C3_CV","")

i = 1
dsf= d
while(i<=20){

dsf[,i] = SMA(d[,i],25)
dsf[,i][is.na(dsf[,i])] = d[,i][is.na(dsf[,i])]
dsf[,i] = ts(dsf[,i],start=0,deltat = deltat(d[,i]))

dsf[,i] = filter(dsf[,i],gkernal(500))
dsf[,i][is.na(dsf[,i])] = d[,i][is.na(dsf[,i])]
dsf[,i] = ts(dsf[,i],start=0,deltat = deltat(d[,i]))

dsf[,i] = SMA(d[,i],11)
dsf[,i][is.na(dsf[,i])] = d[,i][is.na(dsf[,i])]
dsf[,i] = ts(dsf[,i],start=0,deltat = deltat(d[,i]))

i = i+1
}

tlist = bulkprocess(traces[1])

names(tlist) = c("wave","df")
dsf2=(tlist$wave)
plot(dsf2,col="grey",ylim=c(-685,-660),xlim=c(0.1762,0.1767))