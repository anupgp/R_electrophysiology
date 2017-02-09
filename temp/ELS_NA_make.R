# This is for analysis and ploting of ELS data in R
rm(list=ls())
source("~/DATA/ELS/R/loadLibraries.R");
	#----------------------- pre70
pre70_org = read.csv('~/DATA/ELS/CSV/pre70.csv',header=TRUE,stringsAsFactors=FALSE);
# remove the column "X"
pre70_org=pre70_org[,colnames(pre70_org)[!grepl(colnames(pre70_org),pattern="X")]]  
# remove suprious values by changing them to NA
pre70 = pre70_org;
pre70[pre70[,"TauPeak"] > 1,"TauPeak"] = NA; 
pre70[pre70[,"TauTail"] > 1,"TauTail"] = NA; 
# average across sweeps	
avgpre70=ddply(pre70, .(CellID), summarize, FileName=unique(FileName),GroupName=unique(GroupName),BrainRegion=unique(BrainRegion),
ExpDate=unique(ExpDate), BirthDate=unique(BirthDate), TreatTime=mean(TreatTime,na.rm=T), RecTime=mean(RecTime,na.rm=T), VRest = mean(VRest,na.rm=T),
SeriesRes = mean(SeriesRes,na.rm=T), InputRes=mean(InputRes,na.rm=T), TauMem=mean(TauMem,na.rm=T), Sweep=length(Sweep),VCLamp=mean(VClamp,na.rm=T), 
Peak=mean(Peak,na.rm=T),RiseTime=mean(RiseTime,na.rm=T), Tail=mean(Tail,na.rm=T), AreaPeak=mean(AreaPeak,na.rm=T),AreaTail=mean(AreaTail,na.rm=T),
TauPeak=mean(TauPeak,na.rm=T),TauTail=mean(TauTail,na.rm=T),SlopeDirect=mean(SlopeDirect,na.rm=T),SlopeFit=mean(SlopeFit,na.rm=T),Noise=mean(Noise,na.rm=T),
StimNum=mean(StimNum,na.rm=T),StimInterval=mean(StimInterval,na.rm=T))
# postfix colnames with pre70
colnames(avgpre70)[2:length(colnames(avgpre70))]=paste(colnames(avgpre70),rep(".pre70",length(colnames(avgpre70))),sep="")[2:length(colnames(avgpre70))]	
	#----------------------- pre40
pre40_org = read.csv('~/DATA/ELS/CSV/pre40.csv',header=TRUE,stringsAsFactors=FALSE);
# remove the column "X"
pre40_org=pre40_org[,colnames(pre40_org)[!grepl(colnames(pre40_org),pattern="X")]]  
# remove suprious values by changing them to NA
pre40 = pre40_org;
pre40[pre40[,"TauPeak"] > 1,"TauPeak"] = NA; 
pre40[pre40[,"TauTail"] > 1,"TauTail"] = NA; 
# average across sweeps	
avgpre40=ddply(pre40, .(CellID), summarize, FileName=unique(FileName),GroupName=unique(GroupName),BrainRegion=unique(BrainRegion),
ExpDate=unique(ExpDate), BirthDate=unique(BirthDate), TreatTime=mean(TreatTime,na.rm=T), RecTime=mean(RecTime,na.rm=T), VRest = mean(VRest,na.rm=T),
SeriesRes = mean(SeriesRes,na.rm=T), InputRes=mean(InputRes,na.rm=T), TauMem=mean(TauMem,na.rm=T), Sweep=length(Sweep),VCLamp=mean(VClamp,na.rm=T), 
Peak=mean(Peak,na.rm=T),RiseTime=mean(RiseTime,na.rm=T), Tail=mean(Tail,na.rm=T), AreaPeak=mean(AreaPeak,na.rm=T),AreaTail=mean(AreaTail,na.rm=T),
TauPeak=mean(TauPeak,na.rm=T),TauTail=mean(TauTail,na.rm=T),SlopeDirect=mean(SlopeDirect,na.rm=T),SlopeFit=mean(SlopeFit,na.rm=T),Noise=mean(Noise,na.rm=T),
StimNum=mean(StimNum,na.rm=T),StimInterval=mean(StimInterval,na.rm=T))
# postfix colnames with pre40
colnames(avgpre40)[2:length(colnames(avgpre40))]=paste(colnames(avgpre40),rep(".pre40",length(colnames(avgpre40))),sep="")[2:length(colnames(avgpre40))]
	#----------------------- post70
post70_org = read.csv('~/DATA/ELS/CSV/post70.csv',header=TRUE,stringsAsFactors=FALSE);
# remove the column "X"
post70_org=post70_org[,colnames(post70_org)[!grepl(colnames(post70_org),pattern="X")]]  
# remove suprious values by changing them to NA
post70 = post70_org;
post70[post70[,"TauPeak"] > 0.1,"TauPeak"] = NA; 
post70[post70[,"TauTail"] > 1,"TauTail"] = NA;
# average across sweeps	
avgpost70=ddply(post70, .(CellID), summarize, FileName=unique(FileName),GroupName=unique(GroupName),BrainRegion=unique(BrainRegion),
ExpDate=unique(ExpDate), BirthDate=unique(BirthDate), TreatTime=mean(TreatTime,na.rm=T), RecTime=mean(RecTime,na.rm=T), VRest = mean(VRest,na.rm=T),
SeriesRes = mean(SeriesRes,na.rm=T), InputRes=mean(InputRes,na.rm=T), TauMem=mean(TauMem,na.rm=T), Sweep=length(Sweep),VCLamp=mean(VClamp,na.rm=T), 
Peak=mean(Peak,na.rm=T),RiseTime=mean(RiseTime,na.rm=T), Tail=mean(Tail,na.rm=T), AreaPeak=mean(AreaPeak,na.rm=T),AreaTail=mean(AreaTail,na.rm=T),
TauPeak=mean(TauPeak,na.rm=T),TauTail=mean(TauTail,na.rm=T),SlopeDirect=mean(SlopeDirect,na.rm=T),SlopeFit=mean(SlopeFit,na.rm=T),Noise=mean(Noise,na.rm=T),
StimNum=mean(StimNum,na.rm=T),StimInterval=mean(StimInterval,na.rm=T))
# postfix colnames with post70
colnames(avgpost70)[2:length(colnames(avgpost70))]=paste(colnames(avgpost70),rep(".post70",length(colnames(avgpost70))),sep="")[2:length(colnames(avgpost70))]
	#----------------------Merge pre70, pre40
els = merge(avgpre70,avgpre40,by="CellID"); 
	#----------------------Merge with post70
els = merge(els,avgpost70,all.x=TRUE,by="CellID")

	#---------------------Create the additional factors for two factor ANOVA
factors=str_split_fixed(els$GroupName.pre70,"\\+",2)
els = cbind(els,"Treat_invivo"=as.character(factors[,1]),"Treat_exvivo"=as.character(factors[,2]))
	#----------------------Generate new variables
els = cbind(els,"AmpRatio"=abs(els$Tail.pre40/els$Peak.pre70))
els = cbind(els,"AreaRatio"=abs(els$AreaTail.pre40/els$AreaPeak.pre70))
els = cbind(els,"PostPre"=abs(els$Peak.post70/els$Peak.pre70))
expdate=strptime(els[,"ExpDate.pre70"],"%Y%m%d");
birthdate=strptime(els[,"BirthDate.pre70"],"%Y%m%d");
age = as.numeric(difftime(expdate,birthdate,units="days"))
els = cbind(els,"Age"=age)
treattime=strptime(els[,"TreatTime.pre70"],"%H%M");
rectime=strptime(els[,"RecTime.pre70"],"%H%M");
expdelay=as.numeric(difftime(rectime,treattime,units="mins"))/60
els = cbind(els,"ExpDelay"=expdelay)
#------------------------Select cells
select = rep(1,nrow(els));
els2 = cbind(els,"Select"=select)
els2[els2[,"ExpDate.pre70"]>=20121123,"Select"]=0;
els2 = els2[els2[,"Select"]==1,];

	#----------------------Statistics 2-way ANOVA
anovapre70peak=(aov(Peak.pre70~Treat_invivo*Treat_exvivo,els))
anovapre40peak=(aov(Peak.pre40~Treat_invivo*Treat_exvivo,els))
anovapre40tail=(aov(Tail.pre40~Treat_invivo*Treat_exvivo,els))
anovaPostPre=(aov(PostPre~Treat_invivo*Treat_exvivo,els))
anovaNAratioAmp=(aov(AmpRatio~Treat_invivo*Treat_exvivo,els))
anovaNAratioArea=(aov(AreaRatio~Treat_invivo*Treat_exvivo,els))





