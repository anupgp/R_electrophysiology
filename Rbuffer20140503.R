rm(list=ls());
#load("~/DATA/AHP/R/Rdata/HC-CA1_ApTotal_PeakAhpSa_noBADCells.RData")
load("~/DATA/AHP/R/Rdata/OFC-LO_ApTotal_PeakAhpSa_noBADCells.RData")
rawdata = dfy;
rm(list=ls()[ls()!="rawdata"]);
#load("~/DATA/AHP/R/Rdata/HC-CA1_ApTotal-PeakAhpSf-noBADCells.RData")
load("~/DATA/AHP/R/Rdata/OFC-LO_ApTotal-PeakAhpSf-noBADCells.RData")
fitdata = dfy;
rm(list=ls()[ls()!="rawdata" & ls()!="fitdata"]);
sahpdata=merge(rawdata,fitdata,c("FileID","IClamp","GroupName"));
#sahpdata=merge(rawdata,fitdata,c("FileID","IClamp","GroupName","BrainRegion","ApTotal"));
sahplong = reshape(data=sahpdata,varying=c("PeakAhpSa","PeakAhpSf"),direction="long",v.names="PeakAhpS");
sahplong$time = factor(sahplong$time,levels=c(1,2),labels=c("Actual","Fit"),ordered=TRUE);
#legend("topright",legend=c("Actual","Fit"),pch=c("19,21"),col=c("grey50","black"),cex=0.5);
jpeg(paste("~/DATA/AHP/Rebuttal/AHPReviewPlots/OFC-LO_PeakAhpSa+PeakAhpSf_CTRL+CORT_IClamp.jpg",sep=""),width=12,height=12,units="cm",bg="white",res=300)
#Fmfplot(sahplong[sahplong$GroupName=="CTRL+CORT",],"FileID","PeakAhpS","IClamp","time",xpos=-4,ypos=125,seq(0,-4,-1),as.character(seq(0,-4,-1)));
#dev.off();
