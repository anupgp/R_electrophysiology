##--------
badcells_ca1_mgfree = c("20110802C1","20101126C2","20110202C2");
## badcells_ca1_depol = c("20120224C1","20120301C3");
badcells_bla_mgfree = c("20111118C2","20111123C3");
badcells_ppf_7050= c("20120524C1","20120726C2","20120228C4","20120612C2");
badcells_ppf_70100= c("");
## badcells_bla = "";
badcells_mgfree= c(badcells_ca1_mgfree,badcells_bla_mgfree);
## --------
## Levels of EPSC$RecMode: "EPSC70MinusPre", "EPSC70MinusCNQMgFree", "EPSC40Plus", "EPSC70MinusPost", "EPSCIV"
## Levels of EPSC$ExpType: "MgFree", "Depol", "IV", "MgFreeTest"
## ----------------------------------
## Processing EPSC kinetic parameters
## ----------------------------------
ampaktxdat = EPSC[EPSC$BrainRegion == "CA1" & (EPSC$ExpType == "MgFree" | EPSC$ExpType == "MgFreeTestx" | EPSC$ExpType == "Depolx") & EPSC$VRest < -54 & EPSC$avgSeriesres < 80 & EPSC$avgAMnmdaneg < -9 & EPSC$avgMemcap < 450 & (EPSC$RecMode == "EPSC70MinusPre" | EPSC$RecMode == "EPSC70MinusCNQMgFreex") & (!EPSC$CellID%in%badcells_mgfree) & (!EPSC$BirthDate=="20101008") & EPSC$avgAMampa > -450,];
## --------
ampaktxavg = ddply(ampaktxdat,c("CellID"),function(x){dftemp=data.frame("CellID"=unique(x[,"CellID"]),"GroupName"=unique(x[,"GroupName"]),"AMPA.RiseTime"=mean(x[,"RiseTime"],na.rm=TRUE),"AMPA.TauPeak"=mean(x[,"TauPeak"],na.rm=TRUE),"AMPA.TauTail"=mean(x[,"TauTail"],na.rm=TRUE));return(dftemp);});
## -------
nmdaktxdat = EPSC[EPSC$BrainRegion == "CA1" & (EPSC$ExpType == "MgFree" | EPSC$ExpType == "MgFreeTestx" | EPSC$ExpType == "Depolx") & EPSC$VRest < -54 & EPSC$avgSeriesres < 80 & EPSC$avgAMnmdaneg < -9 & EPSC$avgMemcap < 450 & (EPSC$RecMode == "EPSC70MinusPrex" | EPSC$RecMode == "EPSC70MinusCNQMgFree")  & (!EPSC$CellID%in%badcells_mgfree) & (!EPSC$BirthDate=="20101008") & EPSC$avgAMampa > -450,];
## --------
nmdaktxavg = ddply(nmdaktxdat,c("CellID"),function(x){dftemp=data.frame("CellID"=unique(x[,"CellID"]),"GroupName"=unique(x[,"GroupName"]),"NMDA.RiseTime"=mean(x[,"RiseTime"],na.rm=TRUE),"NMDA.TauPeak"=mean(x[,"TauPeak"],na.rm=TRUE),"NMDA.TauTail"=mean(x[,"TauTail"],na.rm=TRUE));return(dftemp);});
## -------
ktxavg=merge(ampaktxavg,nmdaktxavg,by=c("CellID","GroupName"));
## ------------------------------
## Processing for NMDA/AMPA ratios
## ------------------------------
ca1epscdat = EPSC[EPSC$BrainRegion == "CA1" & (EPSC$ExpType == "MgFree" | EPSC$ExpType == "MgFreeTestx" | EPSC$ExpType == "Depolx") & EPSC$VRest < -54 & EPSC$avgSeriesres < 80 & EPSC$avgAMnmdaneg < -9 & EPSC$avgMemcap < 450 & EPSC$TimeIndex == -1 & (!EPSC$CellID%in%badcells_mgfree) & (!EPSC$BirthDate =="20101008") & EPSC$avgAMampa > -450,];
blaepscdat = EPSC[EPSC$BrainRegion == "BLA" & (EPSC$ExpType == "MgFree" | EPSC$ExpType == "MgFreeTestx" | EPSC$ExpType == "Depolx") & EPSC$VRest < -54 & EPSC$avgSeriesres < 80 & EPSC$avgAMnmdaneg < -9 & EPSC$avgMemcap < 450 & EPSC$TimeIndex == -1 & (!EPSC$CellID%in%badcells_mgfree) & (!EPSC$BirthDate =="20101008") & EPSC$avgAMampa > -450,];
blaepscdat = blaepscdat[!is.na(blaepscdat[,1]),];
## ---------
ca1ssdat = nSS[nSS$BrainRegion =="CA1" & nSS$VClamp == -70 & nSS$StimInterval == 50 & nSS$StimNum == 1 & nSS$Sweep == -1 & (!nSS$CellID%in%badcells_ppf_7050) & nSS$VRest < -54,];
ca1ssdat = ca1ssdat[!is.na(ca1ssdat[,"PPFPeak"]),];
## ---------
allepscdat = rbind(ca1epscdat,blaepscdat);
## ---------------------------------------
## Processing for cort data for each animal
## ---------------------------------------
expdates = unique(allepscdat$ExpDate);
## create am empty dataframe to hld cort data from each sample
cortdat = data.frame(CellID=character(),GroupName=character(),CortLevel=integer());
for(expdate in expdates){
    temp = allepscdat[allepscdat[,"ExpDate"]==expdate,c("ExpDate","Invivo","CortLevel")];
    cortdat = rbind(cortdat,temp[1,]);
}
colnames(cortdat)=c("ExpDate","GroupName","CortLevel");
cortdat = cortdat[!is.na(cortdat[,"CortLevel"]),];
## Below lines are commented out 
## --------
## datum = EPSC[EPSC$BrainRegion == "CA1" & (EPSC$ExpType == "MgFreex" | EPSC$ExpType == "MgFreeTest" | EPSC$ExpType == "Depol") & EPSC$VRest < -54 & EPSC$avgSeriesres < 80 & EPSC$TimeIndex == -1 & (!EPSC$CellID%in%badcells_ca1_depol) & EPSC$avgMemcap < 450 & EPSC$avgAMampa > -450,];
## --------
## datum = nSS[nSS$BrainRegion =="CA1" & nSS$VClamp == -70 & nSS$StimInterval == 50 & nSS$StimNum == 1 & nSS$Sweep == -1 & nSS$VRest < -54,];
## datum = nSS[nSS$BrainRegion =="CA1" & nSS$VClamp == -70 & nSS$StimInterval == 100 & nSS$StimNum == 1 & nSS$Sweep == -1 & (!nSS$CellID%in%badcells_ppf_70100) & nSS$VRest < -54,]
## EPSC[EPSC$BrainRegion%in%c("BLA") & EPSC$TimeIndex == -1,c("BirthDate","AnimalBatch","GroupOrd","CortLevel","RecTime","TauMem","narAMmgfree","avgMemcap","avgInputres","avgAMnmdaneg","avgAMampa","avgSeriesres","ExpDelay","Age","AnimalAlone","VRest")]

##------------------------------------------------------
## The below lines select and perform fitting on the EPSC dataframe
##The cells to be filtered out are marked 0 in the Select column
##-------------------------------------------------------

## Two MgFree cells removed
## 20110802C2:CTRL+CORT group very high values of both AMPA and NMDA EPSC amplitude.
## 20101126C2: ELS+VEH group, Very high values of NMDA EPSCs.
## rm_mgfreecells=c("20110802C2","20101126C2");

## 20120613C1: ELS+VEH group, very high NMDA EPSC amplitude.
## 20120131C2: ELS+CORT group, very high NMDA EPSC amplitude
## rm_depolcells=c("20120613C1","20120131C2");

##20130923C1:MgFreeTest & CTRL+VEH group. Very high NMDA EPSC amplitude
## rm_mgfreetestcells=c("20130923C1");

##20111118C2: CTRL+CORT group. Very high NMDA EPSC amplitude
## rm_mgfree_blacells=c("20111118C2");

## rmcells=c(rm_depolcells,rm_mgfreecells,rm_mgfreetestcells,rm_mgfree_blacells);

## EPSC[EPSC$CellID%in%rm_mgfreecells,"SelectMgFreeH"]=0;
## EPSC[EPSC$CellID%in%rm_depolcells,"SelectDepolH"]=0;
## EPSC[EPSC$CellID%in%rm_mgfreetestcells,"SelectMgFreeTestH"]=0;
## EPSC[EPSC$CellID%in%rm_mgfree_blacells,"SelectMgFreeB"]=0;

## EPSC$SelectAllH=as.numeric(EPSC$SelectMgFreeH & EPSC$SelectDepolH & EPSC$SelectMgFreeTestH);

##------------------------------------------------------

##The cells below are filtered based on conditions on the parameters
## EPSC[EPSC$Age<55,"SelectAge"] = 0;
## EPSC[EPSC$RevPeakCor < -15, "SelectRev"] = 0;
## EPSC[EPSC$RevPeakCor > 15, "SelectRev"] = 0;
## EPSC[,"SelectCond"] = as.numeric(EPSC$SelectRev & EPSC$SelectAge);

##-----------------------------------------------------------------------------------------------------------------------------

##Linear fit - AmpNARatioMgFree ~ AmpNARatioDepol
##Linear fit with extra point from averaged values of NARatio from ELS & Depol experiments - AmpNARatioMgFree ~ AmpNARatioDepol
## naratio4fit=EPSC[EPSC$ExpType=="MgFreeTest" & EPSC$Time==0 & EPSC$SelectAllH==1 ,c("CellID","Age","AmpNARatioMgFree","AmpNARatioDepol")];

##Add the averaged values from MgFree & Depol ExpTypes for the CTRL+VEH group
## naratio4fit=rbind(naratio4fit,list("CellID"="20140231C0","Age"=NA,"AmpNARatioMgFree"=mean(EPSC[EPSC$ExpType=="MgFree" & EPSC$BrainRegion=="CA1" & EPSC$Time==0 & EPSC$SelectMgFreeH==1   & EPSC$SelectAge==1 & EPSC$GroupName=="CTRL+VEH","AmpNARatioMgFree"]),"AmpNARatioDepol"=mean(EPSC[EPSC$ExpType=="Depol" & EPSC$BrainRegion=="CA1" & EPSC$Time==0 & EPSC$SelectDepolH==1 & EPSC$SelectAge==1 & EPSC$GroupName=="CTRL+VEH","AmpNARatioDepol"])));

##Linear fitting of NARatiog
## lm_naratiofit=lm(AmpNARatioMgFree ~ AmpNARatioDepol,data=naratio4fit);
## plot(AmpNARatioMgFree ~ AmpNARatioDepol,data=naratio4fit,cex=2);
## abline(lm_naratiofit,col="blue",lwd=2);
## summary(lm_naratiofit);
##Perdiction of AmpNARatioMgFree using lm_fit and AmpNARatioDepol
## EPSC[EPSC$ExpType=="Depol" & EPSC$BrainRegion=="CA1","AmpNARatioMgFree"] = predict.lm(lm_naratiofit,EPSC[EPSC$ExpType=="Depol" & EPSC$BrainRegion=="CA1",c("CellID","ExpType","RecMode","SweepCount","AmpNARatioDepol")]);
##-------------------------------------------------------
##AmpNARatioMgFree without prediction - direct copy of AmpNARatioDepol to AmpNARatioMgFree
##EPSC[EPSC$ExpType=="Depol","AmpNARatioMgFree"] = EPSC[EPSC$ExpType=="Depol","AmpNARatioDepol"];
##------------------------------------------------------
##Get all DEPOL cells(CellID) that are selected based on SelectCond
##Set the value of the SS$Select to 1 if the cell is included in the EPSC dataframe
