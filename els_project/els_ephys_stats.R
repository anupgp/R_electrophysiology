options("width"= 170);
source("/home/anup/goofy/projects/codes/els/R/els_ephys_preprocessing.R");
source("/home/anup/goofy/projects/codes/els/R/els_ephys_processing.R");
## -----------------------------------------------------------------------
epscdat = ca1epscdat;
epscdat = blaepscdat;
ssdat = ca1ssdat;
View(ca1epscdat);
aggregate(narAMmgfree ~ GroupOrd, FUN=function(x){sum(!is.na(x))},data=epscdat);
aggregate(CortLevel ~ GroupOrd, FUN=function(x){sum(!is.na(x))},data=epscdat);
aggregate(PPFPeak ~ GroupOrdered, FUN=function(x){sum(!is.na(x))},data=ssdat);
aggregate(narAMmgfree ~ GroupName, FUN=function(x){mean(x,na.rm=TRUE)},data=epscdat);
aggregate(CortLevel ~ GroupName, FUN=function(x){sum(!is.na(x))},data=cortdat);
## ------------
lmstat2f = lm(narAMmgfree ~ Invivo*Exvivo,data=epscdat);summary(lmstat2f);
aovstat=aov(narAMmgfree ~ Invivo*Exvivo,data=epscdat);summary(aovstat);
lmstat2f = lm(PPFPeak ~ Invivo*Exvivo,data=ssdat);summary(lmstat2f);
aovstat=aov(PPFPeak ~ Invivo*Exvivo,data=ssdat);summary(aovstat);
t.test(CortLevel ~ GroupName, paired=FALSE, var.equal = FALSE, alternative = "two.sided", data=cortdat)
## Check normality on model residual
shapiro.test(residuals(lmstat2f));
## ==============================================
##Check homogeneity of variance
##-----Check if univariate normality assumptions are met
shapiro.test(residuals(lmstat2f));##Check normality on model residual
by(datum$AmpNARatioDepol,datum$GroupOrd,shapiro.test);## Check normality on data of each group
leveneTest(lmstat2f);## A very standard test for homogenity of variance
bartlett.test(AmpNARatioMgFree ~ GroupOrd,data=datum);## Less standard test for homogeneity of variance
qqnorm(lmstat2f$res);
qqline(lmstat2f$res);
mqplot(lmstat2f$res);
hist(lmstat2f$res);
##-----------------------------
bartlett.test(AvgNMDA ~ Groups,data=epscdat);
## Statistics for EPSC kinetics
mean(ktxavg$AMPA.RiseTime,na.rm = TRUE)
t.test(ktxavg[,"AMPA.RiseTime"],ktxavg[,"NMDA.RiseTime"], paired=TRUE, var.equal = FALSE, alternative = "two.sided")
t.test(ktxavg[,"AMPA.TauPeak"],ktxavg[,"NMDA.TauPeak"], paired=TRUE, var.equal = FALSE, alternative = "two.sided")
se(ktxavg$NMDA.TauPeak,na.rm = TRUE)
## Statistics for Cort data
t.test(CortLevel ~ GroupName, paired=FALSE, var.equal = FALSE, alternative = "two.sided", data=cortdat)
## --------------------------
nparmc.stat=nparcomp(AvgNMDA~Groups,data=datum,control="CTRL+VEH",asy.method="probit",type="Dunnett",alternative="two.sided",info=T)
##Perform multiple comparisons with adjusted p-value
TukeyHSD(aovstat);
##--Process RiseTime/AvgTauPeak/AvgTauTail/AvgTauFull CA1/BLA MgFree cells
datum=EPSC[EPSC$BrainRegion=="CA1" & (EPSC$ExpType=="MgFree" | EPSC$ExpType=="MgFreeTest") & EPSC$TimeIndex==-1 & EPSC$SelectOutlier ==1 & EPSC$SelectCond ==1,]
## Select TimeIndex: -1 or 1 to select either AMPA(EPSC70Minus) or NMDA (EPSC70MinusCNQ)
aggregate(AvgRiseTime ~ Groups, FUN=function(x){sum(!is.na(x))},data=datum);
lmstat2f=lm(AmpNARatioDepol ~ Invivo*Exvivo,data=datum);summary(lmstat2f);
boxplot(AmpNARatioMgFree ~ GroupOrd,data=datum);
##-------------------------------------------
## Check normality on data of each group
by(epscdat[,"narAMmgfreeless"],epscdat[,"GroupOrd"],shapiro.test);
pairwise.t.test(epscdat$narAMmgfree, epscdat$GroupOrd,p.adjust.method = "bonf",paired=F,alternative = "two.sided");
pairwise.wilcox.test(epscdat$narAMmgfree, epsc$GroupOrd,p.adjust.method = "bonf",paired=F,alternative = "two.sided");
pairwise.t.test(epscdat$narAMmgfree, epscdat$GroupOrd,p.adjust.method = "bonf",paired=F,alternative = "two.sided");
pairwise.wilcox.test(epscdat[,"narAMmgfreelessnorm"], epscdat[,"GroupOrd"],p.adjust.method = "bonf",paired=F,alternative = "two.sided");
wilcox.test(PPFPeak ~ GroupOrdered,paired=F,data=epscdat[grepl("CTRL\\+VEH",epscdat$GroupName) | grepl("ELS\\+VEH",epscdat$GroupName),],alternative = "two.sided");

kruskal.test(narAMmgfreeless ~ GroupOrd,data=EPSC[EPSC$BrainRegion =="CA1" & (EPSC$ExpType=="MgFree" | EPSC$ExpType=="MgFreeTest") & EPSC$TimeIndex == -1 & (!EPSC$CellID%in%c("20101126C2","20110802C2")),]);
## ------------------
pairwise.t.test(ssdat$PPFPeak, ssdat$GroupOrdered,p.adjust.method = "bonf",paired=F,alternative = "two.sided");
pairwise.wilcox.test(ssdat$PPFPeak, ssdat$GroupOrdered,p.adjust.method = "bonf",paired=F,alternative = "two.sided");
## -----------------
## t-test
t.test(narAMmgfree ~ GroupOrd,alternative = "two.sided",paired = FALSE, var.equal = FALSE, data = epscdat[epscdat[,"Exvivo"]=="VEH",]);
t.test(narAMmgfree ~ GroupOrd,alternative = "two.sided",paired = FALSE, var.equal = FALSE, data = epscdat[epscdat[,"Exvivo"]=="CORT",]);

t.test(PPFPeak ~ GroupName,alternative = "two.sided",paired = FALSE, var.equal = FALSE, data = ssdat[ssdat[,"Exvivo"]=="VEH",]);
t.test(PPFPeak ~ GroupName,alternative = "two.sided",paired = FALSE, var.equal = FALSE, data = ssdat[ssdat[,"Exvivo"]=="CORT",]);

t.test(CortLevel ~ Invivo,alternative = "two.sided",paired = FALSE, var.equal = FALSE, data = epscdat[epscdat[,"Exvivo"]=="VEH",]);
## ----------------------------
## Add column to compute delta for NMDA/AMPA ratio (suggestion by Matthias Smidt)
datum = cbind(datum,narAMmgfreenorm=NA);
datum = cbind(datum,narAMmgfreelessnorm=NA);
datum = cbind(datum,narAMdepolnorm=NA);

datum = cbind(datum,narARmgfreenorm=NA);
datum = cbind(datum,narARmgfreelessnorm=NA);
datum = cbind(datum,narARdepolnorm=NA);

datum$narAMmgfreenorm = datum$narAMmgfree/mean(datum[datum$GroupOrd=="CTRL+VEH","narAMmgfree"],na.rm=T);
datum$narAMmgfreelessnorm = datum$narAMmgfreeless/mean(datum[datum$GroupOrd=="CTRL+VEH","narAMmgfreeless"],na.rm=T);
datum$narAMdepolnorm = datum$narAMdepol/mean(datum[datum$GroupOrd=="CTRL+VEH","narAMdepol"],na.rm=T);

datum$narARmgfreenorm = datum$narARmgfree/mean(EPSC[datum$GroupOrd=="CTRL+VEH","narARmgfree"],na.rm=T);
datum$narARmgfreelessnorm = datum$narARmgfreeless/mean(datum[datum$GroupOrd=="CTRL+VEH","narARmgfreeless"],na.rm=T);
datum$narARdepolnorm = datum$narARdepol/mean(datum[datum$GroupOrd=="CTRL+VEH","narARdepol"],na.rm=T);

datum = cbind(datum,narAMmgfreedelta=NA);
datum = cbind(datum,narAMdepoldelta=NA);

datum$narAMmgfreedelta = (datum$narAMmgfree-mean(EPSC[datum$GroupOrd=="CTRL+VEH","narAMmgfree"],na.rm=T))/mean(datum[datum$GroupOrd=="CTRL+VEH","narAMmgfree"],na.rm=T);
datum$narAMmgfreelessdelta = (datum$narAMmgfreeless-mean(datum[datum$GroupOrd=="CTRL+VEH","narAMmgfreeless"],na.rm=T))/mean(datum[datum$GroupOrd=="CTRL+VEH","narAMmgfreeless"],na.rm=T);
datum$narAMmgdepoldelta = (datum$narAMdepol-mean(datum[datum$GroupOrd=="CTRL+VEH","narAMdepol"],na.rm=T))/mean(datum[datum$GroupOrd=="CTRL+VEH","narAMdepol"],na.rm=T);


## NMDA / AMPA RATIOS
##-------------------------------------------
##Final sel for AmpNARatioDepol from MgFreeDepol+MgFreeTest
boxdepol2=c("20120224C1","20120130C1"); ## boxplot outlier AmpNARatioDepol(>0.5):CTRL+VEH/ELS+VEH
chuckdepol=c("20120613C1");## a wierd cell
boxmgfreeall=c("20101126C2"); ## obtained from the outlier in boxplot of AmpNARatioMgFree(>1):ELS+VEH
##boxdepol1=c("20120130C1","20120224C1","20121127C2","20120127C1"); ## obtained from the outlier in boxplot of AmpNARatioDepol(>0.5):CTRL+VEH & ELS+VEH
boxall=c("20110802C1","20110201C1");
##****
##---------------------------
datum=EPSC[EPSC$BrainRegion=="CA1" & (EPSC$ExpType=="Depol" | EPSC$ExpType=="MgFreeTest" | EPSC$ExpType=="MgFree") & EPSC$SelectDepolH <=1 & EPSC$SelectMgFreeTestH<=1 & EPSC$SelectMgFreeH==1 & EPSC$TimeIndex==-1 & EPSC$SelectAge ==1 & EPSC$RevPeakCor >=-30 & EPSC$RevPeakCor<=30 & (!EPSC$CellID%in%chuckdepol) & (!EPSC$CellID%in%boxdepol2) & (!EPSC$CellID%in%boxall) & (!EPSC$CellID%in%boxmgfreeall),];##For NARatio Choose: SelectDepolH<=1, SelectMgFreeTestH<=1 for final NA ratio data
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
##-----Check if univariate normality assumptions are met
shapiro.test(residuals(lmstat2f));##Check normality on model residual
by(datum$AmpNARatioDepol,datum$GroupOrd,shapiro.test);## Check normality on data of each group
leveneTest(lmstat2f);## A very standard test for homogenity of variance
bartlett.test(AmpNARatioMgFree ~ GroupOrd,data=datum);## Less standard test for homogeneity of variance
qqnorm(lmstat2f$res);
qqline(lmstat2f$res);
qqplot(lmstat2f$res);
hist(lmstat2f$res);
##-----------------------------
plot(lmstat2f$fitted,lmstat2f$res,xlab="Fitted",ylab="Residuals")
nparmc.stat=nparcomp(AvgNMDA~Groups,data=datum,control="CTRL+VEH",asy.method="probit",type="Dunnett",alternative="two.sided",info=T)
##Perform multiple comparisons with adjusted p-value
TukeyHSD(aovstat);
##-------------------------------------
##              POTENTIATION
##-------------------------------------
boxall=c("20110802C1","20110201C1");
boxltp=c("20120229C1");
boxdepol2=c("20120224C1","20120130C1"); ## boxplot outlier AmpNARatioDepol(>0.5):CTRL+VEH/ELS+VEH
chuckdepol=c("20120613C1");## a wierd cell
boxmgfreeall=c("20101126C2"); ## obtained from the outlier in boxplot of AmpNARatioMgFree(>1):ELS+VEH
##---------------------
datumltp=EPSC[EPSC$BrainRegion=="CA1" & (EPSC$ExpType=="Depol") & EPSC$SelectDepolH <=1 & EPSC$TimeIndex==-1 & EPSC$SelectAge ==1 & EPSC$RevPeakCor >=-30 & EPSC$RevPeakCor<=30 & (!EPSC$CellID%in%chuckdepol) & (!EPSC$CellID%in%boxdepol2) & (!EPSC$CellID%in%boxall) & (!EPSC$CellID%in%boxltp),];##For LTP Peak
aggregate(LTPPeak ~ GroupOrd, FUN=function(x){sum(!is.na(x))},data=datumltp);
aggregate(LTPPeak ~ GroupOrd, FUN=mean,data=datumltp);
lmstat2f=lm(LTPPeak ~ Invivo*Exvivo,data=datumltp);summary(lmstat2f);
boxplot(LTPPeak ~ Groups,data=datumltp);
##------------------------Processing nSS --------------------- 23.02.2014
##-------No effects for temporal summation at 50 ms for -70 & 40 mV EPSCs between groups: 26.02.2014
depolcells=unique(EPSC[EPSC$BrainRegion=="CA1" & (EPSC$ExpType=="Depol" | EPSC$ExpType=="MgFreeTestx" | EPSC$ExpType=="MgFreex") & EPSC$SelectDepolH <=1 & EPSC$SelectMgFreeTestH<=1 & EPSC$SelectMgFreeH==1 & EPSC$TimeIndex==-1 & EPSC$SelectAge ==1 & EPSC$RevPeakCor >=-30 & EPSC$RevPeakCor<=30 & (!EPSC$CellID%in%chuckdepol) & (!EPSC$CellID%in%boxdepol2) & (!EPSC$CellID%in%boxall) & (!EPSC$CellID%in%boxmgfreeall),"CellID"]);##For unique cell IDs
##---
sstc=nSS[nSS$CellID%in%depolcells & nSS$VClamp==40 & nSS$StimInterval==100 & nSS$Sweep==-1,];
lineplot.CI(StimNum,NPeak,GroupOrdered,data=sstc,subset=Invivo=="ELS");
##==============================================================================================================
##-70_50:   PPFPeak == No effect in averaged sweep (-1)
ssdatum=nSS[nSS$CellID%in%depolcells & nSS$VClamp==40 & nSS$StimInterval==100 & nSS$Sweep==-1 & nSS$StimNum==1,];
boxplot(PPFPeak ~ GroupOrdered,data=ssdatum);
bargraph.CI(GroupName,PPFPeak,GroupName,data=ssdatum);
lmstat2f=lm(PPFPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
aggregate(PPFPeak ~ GroupName, FUN=function(x){sum(!is.na(x))},data=ssdatum);
## -70_50:   AccoPeak == No effect in averaged sweep
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum);
lmstat2f=lm(AccoPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
bargraph.CI(GroupName,AccoPeak,GroupName,data=ssdatum)
##================================================================================================
##-70_100:   PPFPeak == Invivo effect in first sweep/averaged sweep
ssdatum=nSS[nSS$CellID%in%depolcells & nSS$VClamp==-70 & nSS$StimInterval==100 & nSS$Sweep==-1 & nSS$StimNum==1,];
boxplot(PPFPeak ~ GroupOrdered,data=ssdatum);
bargraph.CI(GroupName,PPFPeak,GroupName,data=ssdatum);
lmstat2f=lm(AccoPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
aggregate(PPFPeak ~ GroupName,FUN=function(x){sum(!is.na(x))},data=ssdatum);
## -70_100:   AccoPeak == Invivo effect in first sweep/averaged sweep
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum);
lmstat2f=lm(AccoPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
bargraph.CI(GroupName,AccoPeak,GroupName,data=ssdatum)
##==============================================================================================
##40_50:   PPFPeak == No effect for averaged/first sweep
ssdatum=nSS[nSS$CellID%in%depolcells & nSS$VClamp==40 & nSS$StimInterval==50 & nSS$Sweep==-1 & nSS$StimNum==1,];
boxplot(PPFPeak ~ GroupOrdered,data=ssdatum);
bargraph.CI(GroupName,PPFPeak,GroupName,data=ssdatum);
lmstat2f=lm(AccoPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
aggregate(AccoPeak ~ GroupName,FUN=function(x){sum(!is.na(x))},data=ssdatum);
aggregate(PPFPeak ~ GroupName,FUN=function(x){sum(!is.na(x))},data=ssdatum);
## 40_50:   AccoPeak == Invivo effect in first sweep/averaged sweep
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum);
lmstat2f=lm(AccoPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
bargraph.CI(GroupOrdered,AccoPeak,GroupName,data=ssdatum)
##==============================================================================================
##40_100:   PPFPeak/AccoPeak == Exvivo effect with averaged sweep
ssdatum=nSS[nSS$CellID%in%depolcells & nSS$VClamp==40 & nSS$StimInterval==100 & nSS$Sweep==-1 & nSS$StimNum==1,];
boxplot(PPFPeak~ GroupOrdered,data=ssdatum);
bargraph.CI(GroupName,PPFPeak,GroupName,data=ssdatum);
lmstat2f=lm(PPFPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
aggregate(PPFPeak ~ GroupName,FUN=function(x){sum(!is.na(x))},data=ssdatum);
## 40_100:   AccoPeak == Invivo effect in first sweep/averaged sweep
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum);
lmstat2f=lm(AccoPeak ~ Invivo*Exvivo,data=ssdatum);summary(lmstat2f);
aggregate(AccoPeak ~ GroupOrdered,FUN=function(x){sum(!is.na(x))},data=ssdatum);
bargraph.CI(GroupOrdered,AccoPeak,GroupName,data=ssdatum)

