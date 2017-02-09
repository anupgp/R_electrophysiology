rm(list=ls());
source("/home/anup/goofy/projects/codes/els/R/loadLibraries.R");
source("/home/anup/goofy/projects/codes/els/R/els_ephys_functions.R");
less_count = 10; 
options(contrasts=c("contr.sum","contr.poly"));
#### LOAD All evoked Epsc's - including MgFreeTest. But 20120613C1 is not in the right order so this is corrected in the csv file.
EPSC = read.csv('/home/anup/goofy/projects/data/els/cooked/ELS_Evoked_All.csv',sep="\t",header=TRUE,stringsAsFactors=FALSE,row.names=NULL);
#### ============================== REMOVE/CHANGE  COLUMNS/VALUES  ================================================

## Remove column "X" 
EPSC=EPSC[,colnames(EPSC)[!grepl(colnames(EPSC),pattern="X")]]; 

## Make a seperate dataframe for averaged Sweeps
EPSCavg = EPSC[EPSC$Sweep ==-1,]; 

## Remove average Sweep from the main data frame
EPSC = EPSC[EPSC$Sweep != -1,]; 

## Change bad Tau values to NA
EPSC[EPSC[,"TauPeak"] >1 | EPSC[,"TauPeak"] < 0,"TauPeak"] = NA; 
EPSC[EPSC[,"TauTail"] >1 | EPSC[,"TauTail"] < 0,"TauTail"] = NA; 
EPSC[EPSC[,"TauFull"] >1 | EPSC[,"TauFull"] < 0,"TauFull"] = NA;

## Change bad peak values to NA
EPSC[EPSC$RecMode=="EPSC40Plus" & EPSC$Peak<=0,"Peak"]=NA;
EPSC[EPSC$RecMode=="EPSC40Plus" & EPSC$Tail<=0,"Tail"]=NA;
EPSC[EPSC$RecMode=="EPSC40Plus" & EPSC$AreaPeak<=0,"AreaPeak"]=NA;
EPSC[EPSC$RecMode=="EPSC40Plus" & EPSC$AreaTail<=0,"AreaTail"]=NA;

EPSC[EPSC$RecMode=="EPSC70MinusPre" & EPSC$Peak>=0,"Peak"]=NA;
EPSC[EPSC$RecMode=="EPSC70MinusPre" & EPSC$AreaPeak>=0,"AreaPeak"]=NA;

EPSC[EPSC$RecMode=="EPSC70MinusCNQMgFree" & EPSC$Peak>=0,"Peak"]=NA;
EPSC[EPSC$RecMode=="EPSC70MinusCNQMgFree" & EPSC$AreaPeak>=0,"AreaPeak"]=NA;
## Change bad resting membrane potentials to 0
EPSC[EPSC[,"VRest"] > -10,"VRest"]  = NA;

## Change zero cort values to NA
EPSC[EPSC[,"CortLevel"] == 0,"CortLevel"   ]  = NA;

## Change ExpDelay values NA to 0
## EPSC[is.na(EPSC[,"ExpDelay"]),"ExpDelay"   ]  = 0;

## Copy the Cor and Uncor values of RevPeak, RevTail to all sweeps of all RecModes of each cell
EPSC = ddply(EPSC,c("CellID"),function(x){
  if("IV"%in%unique(x[,"ExpType"])){
    revpeakcor=mean(x[x$ExpType=="IV","RevPeakCor"]);
    revpeakuncor=mean(x[x$ExpType=="IV","RevPeakUnCor"],na.rm=T);
    revtailcor=mean(x[x$ExpType=="IV","RevTailCor"]);
    revtailuncor=mean(x[x$ExpType=="IV","RevTailUnCor"],na.rm=T);
  }
  else{
    revpeakcor=0;
    revpeakuncor=0;
    revtailcor=0;
    revtailuncor=0;
  }
  x[,"RevPeakCor"] = revpeakcor;
  x[,"RevPeakUnCor"]=revpeakuncor;
  x[,"RevTailCor"] = revtailcor;
  x[,"RevTailUnCor"] = revtailuncor;
  return(x);
}
             );

##  New column for invivo treatment
EPSC = cbind(EPSC,Invivo=unlist(strsplit(EPSC$GroupName,split="+",fixed=T))[seq(1,length(unlist(strsplit(EPSC$GroupName,split="+",fixed=T))),2)]); 
EPSC = cbind(EPSC,Exvivo=unlist(strsplit(EPSC$GroupName,split="+",fixed=T))[seq(2,length(unlist(strsplit(EPSC$GroupName,split="+",fixed=T))),2)]);

## odering the levels of invivo and exvivo columns
EPSC$Invivo = factor(EPSC$Invivo,levels=c("CTRL","ELS"),ordered=T);
EPSC$Exvivo = factor(EPSC$Exvivo,levels=c("VEH","CORT"),ordered=T);

## New column for Groupname
EPSC = cbind(EPSC,GroupOrd=EPSC$GroupName);
EPSC$GroupOrd = factor(EPSC$GroupOrd,levels=c("CTRL+VEH","ELS+VEH","CTRL+CORT","ELS+CORT"),ordered=TRUE);
EPSC = cbind(EPSC,RecModeOrd=factor(EPSC$RecMode,levels=c("EPSC70MinusPre","EPSC40Plus","EPSC70MinusCNQMgFree","EPSC70MinusPost","EPSCIV"),ordered=T));

## New column for animal age
EPSC = cbind(EPSC,Age=as.numeric(difftime(strptime(EPSC[,"ExpDate"],"%Y%m%d"),strptime(EPSC[,"BirthDate"],"%Y%m%d"),units="days")) );

## New column for experimental delay
EPSC = cbind(EPSC,ExpDelay=as.numeric(difftime(strptime(EPSC[,"RecTime"],"%H%M"),strptime(EPSC[,"TreatTime"],"%H%M"),units="mins"))/60 );

## New column for capacitance (pico farads)
EPSC = cbind(EPSC,MemCap = (1/EPSC$SeriesRes + 1/EPSC$InputRes)*EPSC$TauMem*1E12 ); 

## ## New column for CV calculation
## EPSC = ddply(EPSC,c("CellID","RecModeOrd"),function(x){
##     as.data.frame(cbind(x,AmpCVPeak = (var(x[,"Peak"])-var(x[,"Noise"]))/(mean(x[,"Peak"])^2),AmpCVTail = (var(x[,"Tail"])-var(x[,"Noise"]))/(mean(x[,"Tail"])^2) ));
## }
##              );

## Process and  add columns for Time and other averaged values per cell.
EPSC = EPSC_process(EPSC,less_count);

## New column for time index - !!** after running EPSC_process!!
EPSC = cbind(EPSC,Time=EPSC[,"TimeIndex"]*25/60);

## Add the LTP Peak column averaged to the last 3 min (13-15 mins)
EPSC = ddply(EPSC,c("CellID"),function(x){
  dfout=cbind(x,LTPPeak = (mean(x[x$Time>=12 & x$Time<=16,"Peak"],na.rm=T)/mean(x[x$Time<0 & x$Time>-5,"Peak"],na.rm=T))*100,
    LTPArea = (mean(x[x$Time>=12 & x$Time<=16,"AreaPeak"],na.rm=T)/mean(x[x$Time<0 & x$Time>-5,"AreaPeak"],na.rm=T))*100);
  return(dfout);
}
             )

## Add new column to count the cells per day
EPSC = ddply(EPSC,c("ExpDate"),function(x){
  cells = unique(x[,"CellID"]);
  cellindices=c();
  for (i in 1:length(cells)){
    cellindices = c(cellindices,rep(i,dim(x[x$CellID==cells[i],])[1]));
    ## browser()
  }
  as.data.frame(cbind(x,CellCount=cellindices)) });



## ======== Processing MgFree data seperately just for plotting ===========

#### MgFreeTest is only used for time course plotting of Rep traces for Fig1

#### Load all the MgFreeTest cell which also has APV treated control cells (##4)
MgFreeTest = read.csv('/home/anup/goofy/projects/data/els/cooked/ELS_Evoked_MgFreeTestAll.csv',header=TRUE,stringsAsFactors=FALSE,row.names=NULL);

## Remove column "X" 
MgFreeTest=MgFreeTest[,colnames(MgFreeTest)[!grepl(colnames(MgFreeTest),pattern="X")]];

## Remove average Sweep from the main data frame
MgFreeTest = MgFreeTest[MgFreeTest$Sweep != -1,];

#### MgFreeTest is only used for time course plotting of Rep traces for Fig1
MgFreeTest = cbind(MgFreeTest,GroupOrd=factor(MgFreeTest$GroupName,levels=c("CTRL+VEH","CTRL+CORT"),ordered=TRUE));

#### ----> Check: EPSC40Plus is put on the last, unlike for EPSC where it is the second item in RecMode
MgFreeTest = cbind(MgFreeTest,RecModeOrd=factor(MgFreeTest$RecMode,levels=c("EPSC70MinusPre","EPSC70MinusCNQMgFree","EPSC70MinusCNQMgFreeAPV","EPSC40Plus"),ordered=T));

## New colum for membrane capacitance (pico farads)
MgFreeTest = cbind(MgFreeTest,MemCap = (1/MgFreeTest$SeriesRes + 1/MgFreeTest$InputRes)*MgFreeTest$TauMem*1E12 ); 

## New column for CV calculation
MgFreeTest = ddply(MgFreeTest,c("CellID","RecModeOrd"),function(x){
  as.data.frame(cbind(x,AmpCVPeak = (var(x[,"Peak"])-var(x[,"Noise"]) ) / (mean(x[,"Peak"])^2),AmpCVTail =  (var(x[,"Tail"])-var(x[,"Noise"]) )/(mean(x[,"Tail"])^2)	));
}
                   );

## Note: MgFreeTest also has EPSC40Plus as a RecMode, but is omitted here as MgFreeTest is only used for plotting
## Same reason, the order of MgFreeTest$RecModeOrd has EPSC40Plus in the last - different from EPSC$RecModeOrd
MgFreeTest = EPSC_process(MgFreeTest,less_count);

## New column for time index - !!** after running EPSC_process!!
MgFreeTest = cbind(MgFreeTest,Time=MgFreeTest[,"TimeIndex"]*25/60);

#### =====================================================================================================

## Load the temporal summation evoked EPSC files for :-70mV@100ms,-70mV@50ms, 40mV@100ms & 40mV@50ms
SS70100 = read.csv('/home/anup/goofy/projects/data/els/cooked/ELS_ss70100.csv',header=TRUE,stringsAsFactors=FALSE,row.names=NULL,);
SS7050 = read.csv('/home/anup/goofy/projects/data/els/cooked/ELS_ss7050.csv',header=TRUE,stringsAsFactors=FALSE,row.names=NULL);
SS40100 = read.csv('/home/anup/goofy/projects/data/els/cooked/ELS_ss40100.csv',header=TRUE,stringsAsFactors=FALSE,row.names=NULL);
SS4050 = read.csv('/home/anup/goofy/projects/data/els/cooked/ELS_ss4050.csv',header=TRUE,stringsAsFactors=FALSE,row.names=NULL);

## Combine all the SS EPSCs into one.
SS = rbind(SS70100,SS7050,SS40100,SS4050);

## Remove the unwanted colums from SS
SS = SS[,!names(SS)%in%c("X","RevPeak","RevTail")];

## Modify the CellID field to match with the EPSC dataframe: basically shifting the first 2 characters to the last
SS[,"CellID"] = unlist(lapply(SS[,"CellID"],FUN=function(x){return(paste(substring(x,3,nchar(x)),substring(x,1,2),sep=""))}));

## Add a new column Select in the SS
SS = cbind(SS,Select=0);


## Add columns for Invivo and Exvivo Groups
SS = cbind(SS,Invivo=unlist(strsplit(SS$GroupName,split="+",fixed=T))[seq(1,length(unlist(strsplit(SS$GroupName,split="+",fixed=T))),2)]); ##  MAKE A NEW COLUMN for Invivo Treatment
SS = cbind(SS,Exvivo=unlist(strsplit(SS$GroupName,split="+",fixed=T))[seq(2,length(unlist(strsplit(SS$GroupName,split="+",fixed=T))),2)]); ##  MAKE A NEW COLUMN for Invivo Treatment
SS = cbind(SS,GroupOrdered=SS$GroupName); ##  MAKE A NEW COLUMN for GroupOrdered

## Order the Invivo, Exvivo & GroupName columns
SS$Invivo = factor(SS$Invivo,levels=c("CTRL","ELS"),ordered=T);
SS$Exvivo = factor(SS$Exvivo,levels=c("VEH","CORT"),ordered=T);
SS$GroupOrdered = factor(SS$GroupOrdered,levels=c("CTRL+VEH","ELS+VEH","CTRL+CORT","ELS+CORT"),ordered=T);

## Convert SS$StimInterval from ms to secs
## Round off the values of SS$StimInterval to nearest integers
SS$StimInterval = SS$StimInterval*1000;
SS$StimInterval = round(SS$StimInterval);

## Normalize the Peak and Tail EPSCs w.r.t. the first epsc
nSS = ddply(SS,c("CellID","VClamp","StimInterval","Sweep"),function(x){
  dfout = cbind(x,NPeak=x$Peak/x$Peak[1],NTail=x$Tail/x$Tail[1],NAreaPeak=x$AreaPeak/x$AreaPeak[1],
    NAreaTail=x$AreaTail/x$AreaTail[1],PPFPeak = x$Peak[2]/x$Peak[1],PPFTail = x$Tail[2]/x$Tail[1],
    avgPeak1 = rep(x$Peak[1],length(x$Peak)),
    AccoPeak = x$Peak[length(x$Peak)]/x$Peak[1],AccoTail=x$Tail[length(x$Tail)]/x$Tail[1]);
  return(dfout);
}
            )

## Change bad resting membrane potentials to 0
nSS[nSS[,"VRest"] > -10,"VRest"]  = NA;

##Make a new df to hold the values for the Representative plot
##apvcells = c("20130919C1","20130919C2","20130920C2","20130923C3");
##Create a new df to hold peak and tail, also for normalized, in the same column for plotting the APVCELLS
##apvdf = MgFreeTest[MgFreeTest$CellID%in%apvcells,c("CellID","GroupName","ExpType","RecMode","TimeIndex","Time","Peak","Tail")];
##apvdflong = reshape(data=apvdf,direction="long",varying=c("Peak","Tail"),v.names="Val",timevar="Type")
print("fine till here");
## browser();
