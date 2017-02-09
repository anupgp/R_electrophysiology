rm(list=ls())
library("ggplot2")
library("sciplot")
library("stringr")
library("plyr")
	#----------------------- load files
pre70 = read.csv('~/DATA/ELS/ELS_Pre70_new.csv',header=TRUE,stringsAsFactors=FALSE);
pre40 = read.csv('~/DATA/ELS/ELS_Pre40_new.csv',header=TRUE,stringsAsFactors=FALSE);
post70 = read.csv('~/DATA/ELS/ELS_Post70_new.csv',header=TRUE,stringsAsFactors=FALSE);
	# remove the column "X"
pre70=pre70[,colnames(pre70)[!grepl(colnames(pre70),pattern="X")]] 
pre40=pre40[,colnames(pre40)[!grepl(colnames(pre40),pattern="X")]]
post70=post70[,colnames(post70)[!grepl(colnames(post70),pattern="X")]]

ltppre1 = pre70[,c("CellID","GroupName","Sweep","Peak","Tail")]
ltppre1 = cbind(ltppre1,Epoch="Pre-AMPA");
ltppre1 = ddply(ltppre1, "CellID", transform, Time = rev(-seq_along(CellID))*25/60 )



ltppre2 = pre40[,c("CellID","GroupName","Sweep","Peak","Tail")]
ltppre2 = cbind(ltppre2,Epoch="AMPA+NMDA");
ltppre2 = ddply(ltppre2, "CellID", transform, Time = (seq_along(CellID)-1)*25/60 )

ltppost = post70[,c("CellID","GroupName","Sweep","Peak","Tail")]
ltppost = cbind(ltppost,Epoch="Post-AMPA");
ltppost = ddply(ltppost, "CellID", transform, Time = (seq_along(CellID)+20)*25/60 )

ltp = rbind(ltppre1,ltppre2,ltppost)

test = ltp[grepl("Pre-AMPA",ltp[,"Epoch"])==1,]

peakpre70 = ddply(test,.(CellID),function(x)data.frame(Peak=mean(x[,"Peak"],na.rm=TRUE) ) )
#test2 = ddply(ltp,.(CellID,Epoch),function(x) data.frame(c(Peak= x[,"Peak"]/peakpre70$Peak,Time= x[,"Time"][1])) ) )











