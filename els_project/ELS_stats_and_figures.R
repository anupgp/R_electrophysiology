#bst=ls())
#---------------------- 07 July 2015 Statistics on the Object recognition memory data send by Harm
boxplot(Frequency ~ Groups,data=orm);


boxdepol2=c("20120224C1","20120130C1"); # boxplot outlier AmpNARatioDepol(>0.5):CTRL+VEH/ELS+VEH
chuckdepol=c("20120613C1");# a wierd cell
boxmgfreeall=c("20101126C2"); # obtained from the outlier in boxplot of AmpNARatioMgFree(>1):ELS+VEH
#boxdepol1=c("20120130C1","20120224C1","20121127C2","20120127C1"); # obtained from the outlier in boxplot of AmpNARatioDepol(>0.5):CTRL+VEH & ELS+VEH
boxall=c("20110802C1","20110201C1");
#---------------------------
datum=EPSC[(EPSC$BrainRegion=="CA1"| EPSC$BrainRegion=="BLA") & (EPSC$ExpType=="Depol" | EPSC$ExpType=="MgFreeTest" | EPSC$ExpType=="MgFree") & EPSC$SelectDepolH <=1 & EPSC$SelectMgFreeTestH<=1 & EPSC$SelectMgFreeH==1 & EPSC$SelectMgFreeB==1 &  EPSC$TimeIndex==-1 & EPSC$SelectAge ==1 & EPSC$RevPeakCor >=-30 & EPSC$RevPeakCor<=30 & (!EPSC$CellID%in%chuckdepol) & (!EPSC$CellID%in%boxdepol2) & (!EPSC$CellID%in%boxall) & (!EPSC$CellID%in%boxmgfreeall),];#For NARatio Choose: SelectDepolH<=1, SelectMgFreeTestH<=1 for final NA ratio data
#For MgFree Data----------------------------------------------------
#outliermgfreeNMDA=c("20110802C2","20101126C2"); #20110802C2 AvgNMDA<-200, 20101126C2: AvgNMDA < -125
#-----------------
# Generating the data for MgFree experiments
datum=EPSC[EPSC$BrainRegion=="CA1" & (EPSC$ExpType=="MgFree" | EPSC$ExpType=="MgFreeTest") & EPSC$TimeIndex==-1 & EPSC$SelectMgFreeH ==1 & EPSC$SelectMgFreeTestH==1 & EPSC$SelectAge ==1,];
#--------------------------------
datum=EPSC[EPSC$BrainRegion=="BLA" & EPSC$ExpType=="MgFree" & EPSC$TimeIndex==-1 & EPSC$SelectMgFreeB ==1 & EPSC$SelectAge ==1,];
datum[datum$AvgTauPeak>0.2,"AvgTauPeak"]=NA;
datum[datum$AvgTauFull>0.2,"AvgTauPeak"]=NA;
#---------------
#Processing things that are averaged per animal for CA1 MgFree
aggregate(AvgNMDA ~ Invivo, FUN=function(x){sum(!is.na(x))},data=datum[datum$CellCount==1,]);
aggregate(AvgNMDA ~ GroupOrd, FUN=function(x){sum(!is.na(x))},data=datum[datum$CellCount==1,]);
aggregate(AmpNARatioMgFreeDelta ~ GroupOrd, FUN=mean,data=datum[datum$CellCount==1,]);
aggregate(Age ~ Invivo, FUN=range,data=datum[datum$CellCount==1,]);
aggregate(Age ~ Invivo+BrainRegion, FUN=mean,data=datum[datum$CellCount==1,]);
ttest=t.test(CortLevel ~ Invivo, alternative="two.sided",paired=FALSE,data=datum[datum$CellCount==1,]);
#Counting the total number of animals used for both CA1 and BLA
aggregate(CellCount ~ BrainRegion, FUN=function(x){sum(!is.na(x))},data=EPSC[ EPSC$CellCount==1 & EPSC$Time==0 & EPSC$SelectOutlier ==1 & EPSC$SelectCond == 1,]);
# Test 2 factor interaction statistics
lmstat2f=lm(AmpNARatioMgFreeDelta ~ Invivo*Exvivo,data=datum);anova(lmstat2f);
kruskal.test(AmpNARatioMgFreeDelta ~ GroupOrd,data=datum);
lmstat2f=lm(AvgTauPeak ~ Invivo*Exvivo,data=datum);summary(lmstat2f);
lmstat2f=lm(AvgNMDA ~ Invivo*Exvivo,data=datum);summary(lmstat2f);
#lmstat1f=lm(AvgNMDA ~ GroupOrd,data=datum);summary(lmstat1f);
#aovstat=aov(AvgNMDA ~ GroupOrd,data=datum);summary(aovstat);
aggregate(AvgNMDA ~ GroupOrd, FUN=function(x){sum(!is.na(x))},data=datum);
aggregate(AmpNARatioMgFreeDelta ~ GroupOrd, FUN=mean,na.rm=T,data=datum);
boxplot(AmpNARatioMgFreeNorm ~ GroupOrd,data=datum);
#Check homogeneity of variance
bartlett.test(AvgNMDA ~ Groups,data=datum);
nparmc.stat=nparcomp(AvgNMDA~Groups,data=datum,control="CTRL+VEH",asy.method="probit",type="Dunnett",alternative="two.sided",info=T)
#Perform multiple comparisons with adjusted p-value
TukeyHSD(aovstat);
#For MgFree AMPA/NMDA decay time----------------------------------------------------
datum=EPSC[EPSC$BrainRegion=="CA1" & (EPSC$ExpType=="MgFree" | EPSC$ExpType=="MgFreeTest") & EPSC$TimeIndex==1 & EPSC$SelectMgFreeH ==1 & EPSC$SelectMgFreeTestH ==1 & EPSC$SelectAge ==1,]#  EPSC$CellID%in%outliermgfreeNMDA,]
datum[datum$AvgTauPeak>0.2,"AvgTauPeak"]=NA;
aggregate(AvgTauPeak ~ GroupOrd, FUN=function(x){sum(!is.na(x))},data=datum);
aggregate(AvgTauPeak ~ GroupOrd, FUN=mean,data=datum);
lmstat2f=lm(AvgTauPeak ~ Invivo*Exvivo,data=datum);anova(lmstat2f);
lmstat1f=lm(AvgNMDA ~ Groups,data=datum);summary(lmstat1f);
aovstat=aov(AvgNMDA ~ Groups,data=datum);summary(aovstat);
#Check homogeneity of variance
#-----Check if univariate normality assumptions are met
shapiro.test(residuals(lmstat2f));#Check normality on model residual
by(datum$AmpNARatioDepol,datum$GroupOrd,shapiro.test);# Check normality on data of each group
leveneTest(lmstat2f);# A very standard test for homogenity of variance
bartlett.test(AmpNARatioMgFree ~ GroupOrd,data=datum);# Less standard test for homogeneity of variance
qqnorm(lmstat2f$res);
qqline(lmstat2f$res);
mqplot(lmstat2f$res);
hist(lmstat2f$res);
#-----------------------------
bartlett.test(AvgNMDA ~ Groups,data=datum);
nparmc.stat=nparcomp(AvgNMDA~Groups,data=datum,control="CTRL+VEH",asy.method="probit",type="Dunnett",alternative="two.sided",info=T)
#Perform multiple comparisons with adjusted p-value
TukeyHSD(aovstat);
#  Ploting MgFree TauPeak---------------------------
png("~/DATA/ELS/Figures/MgFreeTauPeakNMDA_BLA.png",width=12,height=18,units="cm",bg="transparent",res=300)
par(bty="n");
#par(btx="n");
par(cex.axis=2);
#par(lwd=2);
par(las=2);
#par(col.axis="white");
par(mar=c(4,6,4,2));
lineplot.CI(Groups,AvgTauPeak*1000,Groups,data=datum,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,200),xlim=c(0.6,5),legend=F,axes=F,lwd=2);
boxplot(AvgTauPeak*1000 ~ Groups,data=datum,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
axis(1,at=c(1,2,3,4),labels=as.character(c("","","","")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
axis(2,at=c(0,100,200),labels=as.character(c("0","100","200")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
dev.off();
#--Process RiseTime/AvgTauPeak/AvgTauTail/AvgTauFull CA1/BLA MgFree cells
datum=EPSC[EPSC$BrainRegion=="CA1" & (EPSC$ExpType=="MgFree" | EPSC$ExpType=="MgFreeTest") & EPSC$TimeIndex==-1 & EPSC$SelectOutlier ==1 & EPSC$SelectCond ==1,]
# Select TimeIndex: -1 or 1 to select either AMPA(EPSC70Minus) or NMDA (EPSC70MinusCNQ)
aggregate(AvgRiseTime ~ Groups, FUN=function(x){sum(!is.na(x))},data=datum);
lmstat2f=lm(AmpNARatioDepol ~ Invivo*Exvivo,data=datum);summary(lmstat2f);
boxplot(AmpNARatioMgFree ~ GroupOrd,data=datum);
#-------------------------------------------
# NMDA / AMPA RATIOS
#-------------------------------------------
#Final sel for AmpNARatioDepol from MgFreeDepol+MgFreeTest
boxdepol2=c("20120224C1","20120130C1"); # boxplot outlier AmpNARatioDepol(>0.5):CTRL+VEH/ELS+VEH
chuckdepol=c("20120613C1");# a wierd cell
boxmgfreeall=c("20101126C2"); # obtained from the outlier in boxplot of AmpNARatioMgFree(>1):ELS+VEH
#boxdepol1=c("20120130C1","20120224C1","20121127C2","20120127C1"); # obtained from the outlier in boxplot of AmpNARatioDepol(>0.5):CTRL+VEH & ELS+VEH
boxall=c("20110802C1","20110201C1");
#****
#---------------------------
datum=EPSC[EPSC$BrainRegion=="CA1" & (EPSC$ExpType=="Depol" | EPSC$ExpType=="MgFreeTest" | EPSC$ExpType=="MgFree") & EPSC$SelectDepolH <=1 & EPSC$SelectMgFreeTestH<=1 & EPSC$SelectMgFreeH==1 & EPSC$TimeIndex==-1 & EPSC$SelectAge ==1 & EPSC$RevPeakCor >=-30 & EPSC$RevPeakCor<=30 & (!EPSC$CellID%in%chuckdepol) & (!EPSC$CellID%in%boxdepol2) & (!EPSC$CellID%in%boxall) & (!EPSC$CellID%in%boxmgfreeall),];#For NARatio Choose: SelectDepolH<=1, SelectMgFreeTestH<=1 for final NA ratio data
aggregate(AvgNMDA ~ Invivo*ExpType, FUN=function(x){sum(!is.na(x))},data=datum[datum$CellCount==1,]);
aggregate(AvgTauPeak ~ GroupOrd, FUN=function(x){sum(!is.na(x))},data=datum);
aggregate(AmpNARatioMgFree ~ GroupOrd, FUN=mean,data=datum);
lmstat2f=lm(AvgTauPeak ~ Invivo*Exvivo,data=datum);summary(lmstat2f);
wilcox.test(AmpNARatioMgFree ~ GroupOrd,data=datum,sub=(datum$GroupName=="ELS+CORT" | datum$GroupName=="CTRL+VEH"));
lmstat2f=lm(AmpNARatioMgFree ~ Invivo*Exvivo,data=datum);anova(lmstat2f);
kruskal.test(AmpNARatioDepol ~ GroupOrd,data=datum);
aovstat=aov(AmpNARatioMgFree ~ Invivo*Exvivo,data=datum);summary(aovstat);
boxplot(AmpNARatioMgFree ~ GroupOrd,data=datum);
boxplot(AmpNARatioDepol ~ GroupOrd,data=datum);
#-----Check if univariate normality assumptions are met
shapiro.test(residuals(lmstat2f));#Check normality on model residual
by(datum$AmpNARatioDepol,datum$GroupOrd,shapiro.test);# Check normality on data of each group
leveneTest(lmstat2f);# A very standard test for homogenity of variance
bartlett.test(AmpNARatioMgFree ~ GroupOrd,data=datum);# Less standard test for homogeneity of variance
qqnorm(lmstat2f$res);
qqline(lmstat2f$res);
qqplot(lmstat2f$res);
hist(lmstat2f$res);
#-----------------------------
plot(lmstat2f$fitted,lmstat2f$res,xlab="Fitted",ylab="Residuals")
nparmc.stat=nparcomp(AvgNMDA~Groups,data=datum,control="CTRL+VEH",asy.method="probit",type="Dunnett",alternative="two.sided",info=T)
#Perform multiple comparisons with adjusted p-value
TukeyHSD(aovstat);
# plotting NARatio  -----------------
png("~/DATA/ELS/Figures/AmpNARatioDepolMgFree_CA1.png",width=12,height=18,units="cm",bg="transparent",res=300)
par(bty="n");par(cex.axis=2);par(las=2);par(mar=c(4,6,4,2));
lineplot.CI(Groups,AmpNARatioMgFree,Groups,data=datum,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,0.5),xlim=c(0.6,5),legend=F,axes=F,lwd=2);
boxplot(AmpNARatioMgFree ~ GroupOrd,data=datum,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
axis(1,at=c(1,2,3,4),labels=as.character(c("","","","")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
axis(2,at=c(0,0.25,0.5),labels=as.character(c("0","0.25","0.5")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
#ablineclip(h=100,lty=3,lwd=2,x1=0,x2=4);# Rep Peak
dev.off();
#-------------------------------------
#              POTENTIATION
#-------------------------------------
boxall=c("20110802C1","20110201C1");
boxltp=c("20120229C1");
boxdepol2=c("20120224C1","20120130C1"); # boxplot outlier AmpNARatioDepol(>0.5):CTRL+VEH/ELS+VEH
chuckdepol=c("20120613C1");# a wierd cell
boxmgfreeall=c("20101126C2"); # obtained from the outlier in boxplot of AmpNARatioMgFree(>1):ELS+VEH
#---------------------
datumltp=EPSC[EPSC$BrainRegion=="CA1" & (EPSC$ExpType=="Depol") & EPSC$SelectDepolH <=1 & EPSC$TimeIndex==-1 & EPSC$SelectAge ==1 & EPSC$RevPeakCor >=-30 & EPSC$RevPeakCor<=30 & (!EPSC$CellID%in%chuckdepol) & (!EPSC$CellID%in%boxdepol2) & (!EPSC$CellID%in%boxall) & (!EPSC$CellID%in%boxltp),];#For LTP Peak
aggregate(LTPPeak ~ GroupOrd, FUN=function(x){sum(!is.na(x))},data=datumltp);
aggregate(LTPPeak ~ GroupOrd, FUN=mean,data=datumltp);
lmstat2f=lm(LTPPeak ~ Invivo*Exvivo,data=datumltp);summary(lmstat2f);
boxplot(LTPPeak ~ Groups,data=datumltp);
# plotting LTPPeak  -----------------
png("~/DATA/ELS/Figures/DepolLTPPeak_CA1.png",width=12,height=18,units="cm",bg="transparent",res=300)
par(bty="n");par(cex.axis=2);par(las=2);par(mar=c(4,6,4,2));
lineplot.CI(Groups,LTPPeak,GroupName,data=datumltp,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,200),xlim=c(0.6,5),legend=F,axes=F,lwd=2);
boxplot(LTPPeak ~ Groups,data=datumltp,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
axis(1,at=c(1,2,3,4),labels=as.character(c("","","","")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
axis(2,at=c(0,50,100,150,200),labels=as.character(c("0","50","100","150","200")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
ablineclip(h=100,lty=3,lwd=2,x1=0,x2=4);# Rep Peak
dev.off();
#---------------------------
#---------------------------
tcltp=EPSC[EPSC$BrainRegion=="CA1" & (EPSC$ExpType=="Depol") & EPSC$SelectDepolH <=1 & EPSC$SelectAge ==1 & EPSC$RevPeakCor >=-30 & EPSC$RevPeakCor<=30 & (!EPSC$CellID%in%chuckdepol) & (!EPSC$CellID%in%boxdepol2)& (!EPSC$CellID%in%boxall),]# & (!EPSC$CellID%in%boxltp),];#For LTP Time Course
png("~/DATA/ELS/Figures/Depol_TC_Invivo-CTRL_CA1.png",width=22,height=18,units="cm",bg="transparent",res=300)
par(bty="n");par(cex.axis=2);par(las=2);par(mar=c(4,6,4,2));par(btx="n");
lineplot.CI(Time,NormPeak,GroupName,data=tcltp,,subset=Invivo=="CTRL" & Time>=-5 & Time<=15,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(-100,200),xlim=c(0,49),legend=F,axes=F,lwd=2);
axis(1,at=c(0,12,24,36,48),labels=as.character(c("-5","0","5","10","15")),tick=T,lwd=3,cex.axis=3,xpd=T,las=1,padj=0.6,hadj=0.6)
axis(2,at=c(-100,-50,0,50,100,175),labels=as.character(c("-100","","0","","100","175")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
ablineclip(h=100,lty=3,lwd=2,x1=0,x2=48,col="black");# Rep Peak
dev.off();
#-----------------------------------------------------------------------
#------------------------Processing nSS --------------------- 23.02.2014
#-------No effects for temporal summation at 50 ms for -70 & 40 mV EPSCs between groups: 26.02.2014
depolcells=unique(EPSC[EPSC$BrainRegion=="CA1" & (EPSC$ExpType=="Depol" | EPSC$ExpType=="MgFreeTestx" | EPSC$ExpType=="MgFreex") & EPSC$SelectDepolH <=1 & EPSC$SelectMgFreeTestH<=1 & EPSC$SelectMgFreeH==1 & EPSC$TimeIndex==-1 & EPSC$SelectAge ==1 & EPSC$RevPeakCor >=-30 & EPSC$RevPeakCor<=30 & (!EPSC$CellID%in%chuckdepol) & (!EPSC$CellID%in%boxdepol2) & (!EPSC$CellID%in%boxall) & (!EPSC$CellID%in%boxmgfreeall),"CellID"]);#For unique cell IDs
#---
sstc=nSS[nSS$CellID%in%depolcells & nSS$VClamp==40 & nSS$StimInterval==100 & nSS$Sweep==-1,];
lineplot.CI(StimNum,NPeak,GroupOrdered,data=sstc,subset=Invivo=="ELS");
#==============================================================================================================
#-70_50:   PPFPeak == No effect in averaged sweep (-1)
ssdatum=nSS[nSS$CellID%in%depolcells & nSS$VClamp==40 & nSS$StimInterval==100 & nSS$Sweep==-1 & nSS$StimNum==1,];
boxplot(PPFPeak ~ GroupOrdered,data=ssdatum);
bargraph.CI(GroupName,PPFPeak,GroupName,data=ssdatum);
lmstat2f=lm(PPFPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
aggregate(PPFPeak ~ GroupName, FUN=function(x){sum(!is.na(x))},data=ssdatum);
# plotting nSS -----------------
png("~/DATA/ELS/Figures/AccoPeak7050.png",width=12,height=18,units="cm",bg="transparent",res=300)
par(bty="n");#par(btx="n");par(cex.axis=2);#par(lwd=2);par(las=2);#par(col.axis="white");
par(mar=c(4,6,4,2));
lineplot.CI(GroupOrdered,AccoPeak,GroupOrdered,data=ssdatum,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0.5,3.5),xlim=c(0.6,5),legend=F,axes=F,lwd=2);
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
axis(1,at=c(1,2,3,4),labels=as.character(c("","","","")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
axis(2,at=c(0.5,1.5,2.5,3.5),labels=as.character(c("0.5","1.5","2.5","3.5")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
ablineclip(h=1,lty=3,lwd=2,x1=0,x2=4);# Rep Peak
dev.off();
# -70_50:   AccoPeak == No effect in averaged sweep
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum);
lmstat2f=lm(AccoPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
bargraph.CI(GroupName,AccoPeak,GroupName,data=ssdatum)
#================================================================================================
#-70_100:   PPFPeak == Invivo effect in first sweep/averaged sweep
ssdatum=nSS[nSS$CellID%in%depolcells & nSS$VClamp==-70 & nSS$StimInterval==100 & nSS$Sweep==-1 & nSS$StimNum==1,];
boxplot(PPFPeak ~ GroupOrdered,data=ssdatum);
bargraph.CI(GroupName,PPFPeak,GroupName,data=ssdatum);
lmstat2f=lm(AccoPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
aggregate(PPFPeak ~ GroupName,FUN=function(x){sum(!is.na(x))},data=ssdatum);
# -70_100:   AccoPeak == Invivo effect in first sweep/averaged sweep
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum);
lmstat2f=lm(AccoPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
bargraph.CI(GroupName,AccoPeak,GroupName,data=ssdatum)
# plotting nSS -----------------
png("~/DATA/ELS/Figures/AccoPeak70100.png",width=12,height=18,units="cm",bg="transparent",res=300)
par(bty="n");#par(btx="n");par(cex.axis=2);#par(lwd=2);par(las=2);#par(col.axis="white");
par(mar=c(4,6,4,2));
lineplot.CI(GroupOrdered,AccoPeak,GroupOrdered,data=ssdatum,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0.5,3.5),xlim=c(0.6,5),legend=F,axes=F,lwd=2);
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
axis(1,at=c(1,2,3,4),labels=as.character(c("","","","")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
axis(2,at=c(0.5,1.5,2.5,3.5),labels=as.character(c("0.5","1.5","2.5","3.5")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
ablineclip(h=1,lty=3,lwd=2,x1=0,x2=4);# Rep Peak
dev.off();
#==============================================================================================
#40_50:   PPFPeak == No effect for averaged/first sweep
ssdatum=nSS[nSS$CellID%in%depolcells & nSS$VClamp==40 & nSS$StimInterval==50 & nSS$Sweep==-1 & nSS$StimNum==1,];
boxplot(PPFPeak ~ GroupOrdered,data=ssdatum);
bargraph.CI(GroupName,PPFPeak,GroupName,data=ssdatum);
lmstat2f=lm(AccoPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
aggregate(AccoPeak ~ GroupName,FUN=function(x){sum(!is.na(x))},data=ssdatum);
aggregate(PPFPeak ~ GroupName,FUN=function(x){sum(!is.na(x))},data=ssdatum);
# plotting nSS -----------------
png("~/DATA/ELS/Figures/AccoPeak4050.png",width=12,height=18,units="cm",bg="transparent",res=300)
par(bty="n");#par(btx="n");par(cex.axis=2);#par(lwd=2);par(las=2);#par(col.axis="white");
par(mar=c(4,6,4,2));
lineplot.CI(GroupOrdered,AccoPeak,GroupOrdered,data=ssdatum,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,5),xlim=c(0.6,5),legend=F,axes=F,lwd=2);
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
axis(1,at=c(1,2,3,4),labels=as.character(c("","","","")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
axis(2,at=c(0,2.5,5),labels=as.character(c("1","2.5","5")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
ablineclip(h=1,lty=3,lwd=2,x1=0,x2=4);# Rep Peak
dev.off();
# 40_50:   AccoPeak == Invivo effect in first sweep/averaged sweep
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum);
lmstat2f=lm(AccoPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
bargraph.CI(GroupOrdered,AccoPeak,GroupName,data=ssdatum)
#==============================================================================================
#40_100:   PPFPeak/AccoPeak == Exvivo effect with averaged sweep
ssdatum=nSS[nSS$CellID%in%depolcells & nSS$VClamp==40 & nSS$StimInterval==100 & nSS$Sweep==-1 & nSS$StimNum==1,];
boxplot(PPFPeak~ GroupOrdered,data=ssdatum);
bargraph.CI(GroupName,PPFPeak,GroupName,data=ssdatum);
lmstat2f=lm(PPFPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
aggregate(PPFPeak ~ GroupName,FUN=function(x){sum(!is.na(x))},data=ssdatum);
# 40_100:   AccoPeak == Invivo effect in first sweep/averaged sweep
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum);
lmstat2f=lm(AccoPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
aggregate(AccoPeak ~ GroupOrdered,FUN=function(x){sum(!is.na(x))},data=ssdatum);
bargraph.CI(GroupOrdered,AccoPeak,GroupName,data=ssdatum)
# plotting nSS -----------------
png("~/DATA/ELS/Figures/AccoPeak40100.png",width=12,height=18,units="cm",bg="transparent",res=300)
par(bty="n");#par(btx="n");par(cex.axis=2);#par(lwd=2);par(las=2);#par(col.axis="white");
par(mar=c(4,6,4,2));
lineplot.CI(GroupOrdered,AccoPeak,GroupOrdered,data=ssdatum,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,3),xlim=c(0.6,5),legend=F,axes=F,lwd=2);
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
axis(1,at=c(1,2,3,4),labels=as.character(c("","","","")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
axis(2,at=c(0,1,2,3),labels=as.character(c("0","1","2","3")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
ablineclip(h=1,lty=3,lwd=2,x1=0,x2=4);# Rep Peak
dev.off();
