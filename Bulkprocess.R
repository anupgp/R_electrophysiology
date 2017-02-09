#==========================================================================
bulkprocess <- function(fnames){
	source("~/DATA/R/R_scripts/Clampex.R",echo=F)
	source("~/DATA/R/R_scripts/peakalgo.R",echo=F)
	source("~/DATA/R/R_scripts/Dataframetemplates.R",echo=F)
	source("~/DATA/R/R_scripts/SimpsonsAreaFunction.R",echo=F)

	f.abfload = match.fun("abfload")
	f.gkernal = match.fun("gkernal")
	f.peaks = match.fun("peaks")
	f.createdf = match.fun("createdf")
	f.area = match.fun("simpsonsarea")

	dff = f.createdf()
#     -------------  Starting times for spike counts
	pstart = 0.65
	pend = 1.28
#	-------------- Staring times for baseline
	bstart = 0
	bend = 0.165
#	-------------- Starting times for series res
	srstart = 0.1762
	srend = 0.1767
#	--------------- 
	i=1
	n = 1
	while(i<=length(fnames)){
		
		dd = f.abfload(fnames[i],path=dirname(fnames[i]),"10 Vm")
		j =1
#               	------------------------ Scale the data
			if(mean(dd)> -10) {dd = dd*100}

		while(j<=dim(dd)[2]){
			dff[n,"filename"] = sub(pattern="[.abf]{4}",x=basename(fnames[i]),"")
			dff[n, "traceid"] = j
			dff[n,"group"] = gsub(pattern="([A-Z0-9]{9})([_]{1})([C1-9]{2})([_]{1})",x=dff[n,"filename"],"")
#                 ----------------------- Filtering
			d = dd[,j]
#			---
			d1 = SMA(d,25)
			d1[is.na(d1)] = d[is.na(d1)]
			d1 = ts(d1,start=0,deltat = deltat(d))
#			---
			d2 = filter(d1,gkernal(500))
			d2[is.na(d2)] = d1[is.na(d2)]
			d2 = ts(d2,start=0,deltat = deltat(d))
#			---
			d3 = SMA(d2,11)
			d3[is.na(d3)] = d2[is.na(d3)]
			d3 = ts(d3,start=0,deltat = deltat(d))
#			------------------------- Checking for equal lengths
			if(length(d)!=length(d1) || length(d1)!=length(d2)|| length(d2)!=length(d3)||length(d3)!=length(d)){
				break;
				print("sorry length of one or more time series are not equal")
			}
#			------------------------ Series resistance
			dff[n,"sres"] = abs(min(window(d1,0.1762,0.1767))-min(window(d1,0.1762,0.1767)))/20
#			------------------------ Getting Vm (baseline voltage)
			dff[n,"vm"] = mean(window(d3,bstart,bend))
#			------------------------ Getting baseline substracted trace for AHP			
			dahp = d3-dff[n,"vm"]
#			------------------------ Get mAHP & sAHP values from substracted trace
			mahps = f.peaks(window(dahp,1.28,1.46),delta=5)
			dff[n,"mAHPp"] = mahps[1,4]
			dff[n,"mAHPt"] = mahps[1,3]+1.28*(mahps[1,4]<0)
			dff[n,"mAHPa"] = f.area(dahp,1.3,dff[n,"mAHPt"]+0.150,0.01)*(mahps[1,4]<0)# area units 10mv*s
			dff[n,"sAHPp"] = mean(window(dahp,dff[n,"mAHPt"]+0.300,dff[n,"mAHPt"]+0.350))*(mahps[1,4]<0)# window of average = 0.050 sec
			dff[n,"sAHPa"] = f.area(dahp,dff[n,"mAHPt"]+0.300,5,0.01)*(mahps[1,4]<0) # area units 10mv*s
			dff[n,"sAHPt"] = (mean(c(0.150,0.2))+ dff[n,"mAHPt"])*(mahps[1,4]<0)

#			------------------------ Spikes and dips 
			aps = f.peaks(window(d,pstart,pend),delta=100)
			aps[,1][aps[,1]>0] = aps[,1][aps[,1]>0]+pstart
			aps[,3][aps[,3]>0] = aps[,3][aps[,3]>0]+pstart
			dff[n,grep("peakx",names(dff))][1:dim(aps)[1]] = aps[,1]
			dff[n,grep("peaky",names(dff))][1:dim(aps)[1]] = aps[,2]
			dff[n,"peak"] = length(aps[,2][aps[,2]>0])
#			------------------------
			dff[n,grep("dipx",names(dff))][1:dim(aps)[1]] = aps[,3]
			dff[n,grep("dipy",names(dff))][1:dim(aps)[1]] = aps[,4]
			dff[n,"dip"] = length(aps[,4][aps[,4]<0])
			
			dff = rbind(dff,f.createdf())		
			j = j+1
			n = n+1
#			rm(d,d1,d2,d3,dahp)
		}
		i=i+1
		closeAllConnections()
	}
	rm(list = c("f.peaks","f.abfload","f.gkernal","f.createdf","f.area"))
	dff = dff[nchar(dff[,"filename"])>0,]
return(dff)
}
#============================================================================================

