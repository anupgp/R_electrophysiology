if (!exists("AHP"))
	{
	source("~/DATA/AHP/R/AHP_Load.R");
	}
# ------------- Change the values of all columns with Select as 1
for (i in grep(colnames(AHP),pat="Select",value=T)) AHP[,i] = 1;
# ------------- Bad cells that need to be completely removed from the analysis
#		HC-CA1
badcells_HC = c("SEP132011_C4_CC_HP20110913");
#		OFC-LO
badCVcells_LO = c("AUG012012AHP_C1_CV_HP20120801","AUG212012AHP_C3_CV_HP20120821");
badCCcells_LO = c("AUG032012AHP_C9_CC_HP20120803");
badcells_LO = c(badCCcells_LO,badCVcells_LO);
#		PFC-PL
badCVcells_PL = c("C120121102_C1_0001","C120121119_C1_0001","C120121228_C1_0001","C220121102_C2_0002",
			"C220121109_C2_0001","C220121228_C2_0001","C420121120_C4_0001","C420121211_C4_0001");
badCCcells_PL = c("C120121120_C1_0001","C520121113_C5_0001");
badcells_PL = c(badCCcells_PL,badCVcells_PL);
#		PFC-IL
badCVcells_IL = c("C120121219_C1_0001","C120130104_C1_0001","C220121113_C2_0008","C220130104_C2_0002","C320121109_C3_0001");#"C320130107_C3_0001"
badCCcells_IL = c("C220121211_C2_0001","C320121120_C3_0001","C320121211_C3_0001");	#"C620130109_C6_0001"
badcells_IL = c(badCCcells_IL,badCVcells_IL);
# All bad cells
badcells = c(badcells_HC, badcells_LO,badcells_PL,badcells_IL);
# The selection criterias
#		HC-CA1
	AHP[ (AHP$FileID %in% badcells),"SelectBad"] = 0; # Completly badcells
	AHP[AHP$AvgInputResf < 100E06 | AHP$AvgInputResf >450E06 ,"SelectIR" ] = 0;#100/450
	AHP[AHP$AvgSeriesRes < 15E06 | AHP$AvgSeriesRes >45E06 ,"SelectSR"] = 0; #15/45
	#AHP[(AHP$ApMax < 10 | AHP$ApMax > 100),"SelectAp"] = 0; # 5/60
# Indicate any selction in coulmn select
AHP[,"Select"] = as.numeric(AHP[,"SelectBad"] & AHP[,"SelectIR"] & AHP[,"SelectSR"] & AHP[,"SelectAp"]);
##===========================================================================
myroi="OFC-LO";
#xparam="ApTotal";
yparam="PeakAhpSa";
##=============================================================
#dfx=AHP[AHP$BrainRegion==myroi & AHP$Select==1 & AHP$Count==1 & AHP$IClamp>=150 & AHP$IClamp<=450,c("FileID",xparam,"IClamp","GroupName")];
dfy=AHP[AHP$BrainRegion==myroi & AHP$Select==1 & AHP$Count==1 & AHP$IClamp>=150 & AHP$IClamp<=450,c("FileID",yparam,"IClamp","GroupName")];
# Remove any obervations that has NA
#dfx = dfx[!is.na(dfx[,xparam]),];
dfy = dfy[!is.na(dfy[,yparam]),];
#-------------------------------
#dfx2 = FoutlierAmeliaAov(dfx,"FileID",xparam,"IClamp","GroupName");
dfy2 = FoutlierAmeliaAov(dfy,"FileID",yparam,"IClamp","GroupName");
#---------------------------
#xpvalue=Fanovamix(dfx2,"FileID",xparam,"IClamp","GroupName");
ypvalue=Fanovamix(dfy2,"FileID",yparam,"IClamp","GroupName");
#------------------------------
#xmfmodel = Flmer(dfx2,"FileID",xparam,"IClamp","GroupName");
ymfmodel = Flmer(dfy2,"FileID",yparam,"IClamp","GroupName");
#----------------------------
#xpower=Flmerpower(dfx2,xmfmodel,1000);
ypower=Flmerpower(dfy2,ymfmodel,1000);
#-------------------------------
#xpowerN = FlmerpowerN(dfx2,xparam,xmfmodel,nsim=100,10)
ypowerN = FlmerpowerN(dfy2,yparam,ymfmodel,nsim=100,10)
##=============================================================
#dfxy=cbind(merge(dfx2,dfy2,c("FileID","IClamp","GroupName")),"BrainRegion"=myroi);
#------------------------------------------------------------------------------------------
#load("~/DATA/AHP/R/Rdata/HC-CA1_ApTotal-PeakAhpSf-noBADCells.RData")
#load("~/DATA/AHP/R/Rdata/HC-CA1_ApTotal-PeakAhpMf-noBADCells.RData")
#load("~/DATA/AHP/R/Rdata/OFC-LO_ApTotal-PeakAhpMf-noBADCells.RData")
#load("~/DATA/AHP/R/Rdata/OFC-LO_ApTotal-PeakAhpSf-noBADCells.RData")
#load("~/DATA/AHP/R/Rdata/PFC-IL_ApTotal-PeakAhpMf-noBADCells.RData")
#load("~/DATA/AHP/R/Rdata/PFC-IL_ApTotal-PeakAhpSf-noBADCells.RData")
#load("~/DATA/AHP/R/Rdata/PFC-PL_ApTotal-PeakAhpMf-noBADCells.RData")
#load("~/DATA/AHP/R/Rdata/PFC-PL_ApTotal-PeakAhpSf-noBADCells.RData")
avgxparam = Ffunperblock(dfxy,"FileID",xparam,"IClamp","GroupName","mean");
avgxparam = cbind(avgxparam,GroupName=rownames(avgxparam));
avgxparam = reshape(data=avgxparam,varying=as.character(seq(150,450,25)),v.names=xparam,timevar="IClamp",times=as.character(seq(150,450,25)),direction="long");
avgyparam = Ffunperblock(dfxy,"FileID",yparam,"IClamp","GroupName","mean");
avgyparam = cbind(avgyparam,GroupName=rownames(avgyparam));
avgyparam = reshape(data=avgyparam,varying=as.character(seq(150,450,25)),v.names=yparam,timevar="IClamp",times=as.character(seq(150,450,25)),direction="long");
avgxy=merge(avgxparam,avgyparam,by=c("GroupName","IClamp","id"));
cor.test(avgxy[avgxy$GroupName=="CTRL+CORT","ApTotal"],avgxy[avgxy$GroupName=="CTRL+CORT","PeakAhpSf"],method="pearson",alt="two.sided")
#Fmfplot(dfy2,"FileID","PeakAhpSf","IClamp","GroupName",xpos=-3,ypos=125,seq(-0,-3,-1),as.character(seq(-0,-3,-1)));
#Fmfplot(dfy2,"FileID","PeakAhpMf","IClamp","GroupName",xpos=-7,ypos=125,seq(-1,-7,-2),as.character(seq(-1,-7,-2)));
#------------------------------------------------------------------------------------------
#jpeg(paste("~/DATA/AHP/Rebuttal/AHPReviewPlots/",myroi,"-",xparam,"-",yparam,".jpg",sep=""),width=15,height=15,units="cm",bg="white",res=300)
#xyplot = Fxyerror(dfxy,"FileID",xparam,yparam,"IClamp","GroupName");
#dev.off();
#jpeg(paste("~/DATA/AHP/Rebuttal/AHPReviewPlots/",myroi,"_",yparam,"_IClamp.jpg",sep=""),width=12,height=12,units="cm",bg="white",res=300)
#Fmfplot(dfx2,"FileID",xparam,"IClamp","GroupName",xpos=20,ypos=125,seq(20,65,15),as.character(seq(20,65,15)));
#dev.off();
#------------------------------------------------------------------------------------------
tableparams = c("VRest","InputResf","TauMem","ApAmp","ApRiseTime","ApSlope","ApThres","ApHalfWidth");
table=Fmaketable(AHP[AHP$BrainRegion=="OFC-LO" & AHP$Count==1 & AHP$Select==1 & AHP$IClamp >=150 & AHP$IClamp <= 250,] ,"FileID",tableparams,"IClamp","GroupName","mean");
table;
#-------------------------------------
#df2y=Ffindmissing(dfy,"FileID",myyvar,"IClamp","GroupName");
#df3y=Famelia(df2y,"FileID",myyvar,"IClamp","GroupName",500);
#Fanovamix(df3y,"FileID",myyvar,"IClamp","GroupName");
#mforg=lmer(PeakAhpSf  ~ GroupName*IClamp + (1|IClamp)+(1|FileID),data=dfy);
#aemforg=influence(mforg,obs=TRUE);
#dfyck=cbind(dfy,"CooksD"=cooks.distance(aemforg));
#lmerControl(optimizer="Nelder_Mead",optCtrl=list());
#mffullmodel=lmer(PeakAhpSf ~ GroupName*IClamp + (1 + IClamp | FileID),data=dfy);
#mffullmodel=lmer(PeakAhpSf ~ IClamp*GroupName + (1+IClamp|FileID),data=dfy);
#mfmemodel=lmer(PeakAhpSf ~ IClamp+GroupName + (1+IClamp|FileID),data=dfy);
#vc=VarCorr(mffullmodel);
#Get residual standard deviaiton: attr(vc,"sc");
#Get standard deviation of random effect (IClamp): as.numeric(attr(vc$FileID,"stddev")["IClamp"])
#Get standard deviation of intercept: as.numeric(attr(vc$FileID,"stddev")["(Intercept)"])
#print(vc,comp=c("Variance"),digits=2);
#test3xy=merge(test3x,test3y,c("FileID","IClamp","GroupName"));
#test3xy=cbind(test3xy,"BrainRegion"=myroi);
#dfAmeliaMI(AHP[AHP$BrainRegion=="OFC-LO" & AHP$Count==1 & AHP$Select==1 & AHP$IClamp >=125 & AHP$IClamp <= 450,],"FileID","SpkIntLast","IClamp","GroupName",500,"yes",sdtimes=2.7)
#aggregate(InputResf ~ GroupName,mean, data=aggregate(InputResf ~ FileID+GroupName,mean,data=AHP[AHP$BrainRegion=="HC-CA1" & AHP$Select==1 & AHP$Count==1 & AHP$IClamp>=125 & AHP$IClamp<=450,]))

#t.test(InputResf ~ GroupName,paired=F, aggregate(InputResf ~ FileID+GroupName,mean,data=AHP[AHP$BrainRegion=="HC-CA1" & AHP$Select==1 & AHP$Count==1 & AHP$IClamp>=125 & AHP$IClamp<=450,]))
#ahpplot=lineplotmf(ahp[[1]],"FileID","IClamp","GroupName","SpkIntLF",0,125,c(0,50,100),c("0","50","100"))ahpplot;
#
#ahp=AHP[AHP$BrainRegion=="OFC-LO" & AHP$Count==1 & AHP$AvgInputResf>300E06 & AHP$SelectSR==1 & AHP$SelectOther<=1 & AHP$SelectAp==1 & AHP$IClamp >=125 & AHP$IClamp <= 450,];
