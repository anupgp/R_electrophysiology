#----------------------- load IV CSV
iv = read.csv('~/DATA/ELS/CSV/ELS_IV.csv',header=TRUE,stringsAsFactors=FALSE);
# remove the column "X"
iv = iv[,colnames(iv)[!grepl(colnames(iv),pattern="X")]];
# average across sweeps	
avgiv=ddply(iv, .(CellID), summarize, FileName=unique(FileName),GroupName=unique(GroupName),BrainRegion=unique(BrainRegion),
ExpDate=unique(ExpDate), BirthDate=unique(BirthDate), TreatTime=mean(TreatTime,na.rm=T), RecTime=mean(RecTime,na.rm=T), VRest = mean(VRest,na.rm=T),
SeriesRes = mean(SeriesRes,na.rm=T), InputRes=mean(InputRes,na.rm=T), TauMem=mean(TauMem,na.rm=T), Sweep=length(Sweep),
RevPeakUncor = mean(RevPeakUncor,na.rm=T),RevTailUncor=mean(RevTailUncor,na.rm=T),RevPeakCor = mean(RevPeakCor,na.rm=T),RevTailCor=mean(RevTailCor,na.rm=T))
# postfix colnames with .iv
colnames(avgiv)[2:length(colnames(avgiv))]=paste(colnames(avgiv),rep(".iv",length(colnames(avgiv))),sep="")[2:length(colnames(avgiv))];
source("~/DATA/ELS/R/ELS_NA_make.R");
if (exists("els") == 0) stop(' !!!!!!!   Excecution stopped. dataframe: "els" missing    !!!!!!');
ivna = merge(avgiv,els,by="CellID");
  
	
