#Plots for JNS Figure 1 Panel 1
lineplot.CI(Time,Peak,data=EPSC[EPSC$CellID=="20120120C1",]);# A good MgFree CTRL+VEH cell = 20120120C1
lineplot.CI(Time,Peak,data=EPSC[EPSC$CellID=="20120716C1",]);# A good Depol CTRL+VEH cell = 20120716C1
lineplot.CI(Time,Peak,data=EPSC[EPSC$CellID=="20130920C1",]);# A good MgFreeTest CTRL+VEH cell = 20120716C1
lineplot.CI(Time,Peak,group=CellID,data=EPSC[EPSC$ExpType=="MgFreeTest" & EPSC$GroupName=="CTRL+VEH" & EPSC$SelectOutlier==1 & EPSC$SelectCond==1,]);
#Plot NARatios from the MgFree & MgFreeTest experiments for selected cells (with outliers & conditions)
bargraph.CI(Groups,AvgPeak,group=Groups,data=EPSC[EPSC$RecMode=="EPSC70MinusPre" & EPSC$BrainRegion=="BLA" & (EPSC$ExpType=="MgFreeTest"| EPSC$ExpType=="MgFree" | EPSC$ExpType=="Depol") & EPSC$SelectOutlier==1 & EPSC$SelectCond==1 & EPSC$TimeIndex==-1,],space=0.5,cex.leg=1.5,cex.names=1.5,cex.lab=1.5,main="All cells",cex.axis=2)
#---------
bargraph.CI(GroupName,NARatioMgFree,group=GroupName,data=EPSC[EPSC$ExpType=="MgFree" & EPSC$AvgPeak < -20 & EPSC$NARatioMgFree<0.5 & EPSC$TimeIndex==-1,])
bargraph.CI(GroupName,NARatioDepol,group=GroupName,data=EPSC[EPSC$ExpType=="Depol" & EPSC$AvgPeak > 0 & EPSC$TimeIndex==0 & EPSC$RevPeakCor <40 & EPSC$RevPeakCor>-40,])
#bargraph.CI(Invivo,CortLevel,group=Invivo,data=EPSC[EPSC$TimeIndex==-1 & EPSC$CellCount ==1 & EPSC$ExpDate != 20110523,]);
jpeg("~/DATA/ELS/Rplots/CortLevel~Invivo_CellsWithCondBLA.jpg",width=10,height=15,units="cm",bg="white",res=300);
par(mar=c(5,5,5,5));
bargraph.CI(Invivo,CortLevel,group=Invivo,data=EPSC[EPSC$BrainRegion=="BLA" & EPSC$TimeIndex==-1 & EPSC$CellCount ==1 & EPSC$SelectOutlier==1 & EPSC$SelectCond==1,],col=c("white","lightblue"),legend=T,ylab=c("Corticosterone level (ng/ml)"),ylim=c(0,40),space=0.05,cex.leg=1,cex.names=1.5,cex.lab=2,main="All selected cells (BLA)",cex.axis=1.5,axes=T,x.leg=0,y.leg=0.5);
#title(ylab="Corticosterone level (ng/ml)",mgp=c(4,1,0));
dev.off();
aggregate(CortLevel ~ ExpDate+Groups,FUN=mean,data=EPSC2[EPSC$TimeIndex==-1 & EPSC$CellCount ==1 & EPSC$CellID != 20110523,]);
epsc=dfOutlierNA("CellID","AmpNARatioDepol","Groups",2,EPSC[EPSC$TimeIndex==-1 & EPSC$AnimalBatch<=4,]);
bargraph.CI(Groups,AmpNARatioDepol,Groups,data=epsc,legend=T,space=0.1)
summary(lm(AmpNARatioDepol ~ Invivo+Exvivo+Invivo*Exvivo,data=epsc));
naratiotestdepol=abs(EPSC[EPSC$ExpType=="MgFreeTest" & EPSC$RecMode=="EPSC40Plus" & EPSC$SweepCount==1,"AvgTail"]/EPSC[EPSC$ExpType=="MgFreeTest" & EPSC$RecMode=="EPSC70MinusPre" & EPSC$SweepCount==1,"AvgPeak"]);
naratiotestmgfree=abs(EPSC[EPSC$ExpType=="MgFreeTest" & EPSC$RecMode=="EPSC70MinusCNQMgFree" & EPSC$SweepCount==1,"AvgPeak"]/EPSC[EPSC$ExpType=="MgFreeTest" & EPSC$RecMode=="EPSC70MinusPre" & EPSC$SweepCount==1,"AvgPeak"]);
jpeg("~/DATA/ELS/Rplots/Time-Peak_MgFreeTest_CTRL+VEH.jpg",width=25,height=15,units="cm",bg="white",res=300);
lineplot.CI(Time,Peak,CellID,data=EPSC[EPSC$ExpType=="MgFreeTest" & EPSC$Groups=="CTRL+VEH",],type="b",xlab="Time",ylab="Peak (amplitude (pA)",main="MgFreeTest cells - CTRL+VEH group");
lines(c(-9,length(unique(EPSC[EPSC$ExpType=="MgFreeTest","TimeIndex"]))),c(0,0),lty=3,col="blue",lwd=2)
dev.off();
#--------------------Correlation plot MgFreeTest AmpNARatioDepol~AmpNARatioMgFree
jpeg("~/DATA/ELS/Rplots/Fit_AmpNARatioMgFree~AmpNARatioDepol_SelCellsWithAvg_CTRL+VEH.jpg",width=15,height=15,units="cm",bg="white",res=300);
plot(AmpNARatioMgFree ~ AmpNARatioDepol,data=naratio4fit,cex=2);
text(AmpNARatioMgFree ~ AmpNARatioDepol,data=naratio4fit[naratio4fit$CellID=="20140231C0",],"X");
abline(lm_naratiofit,col="blue",lwd=2);
summary(lm_naratiofit)
summary(lm_naratiofit)$coefficients
summary(lm_naratiofit)$r.squared
#text(0.1,0.25,c(names(summary(lm(AmpNARatioMgFree ~ AmpNARatioDepol,data=EPSC[EPSC$ExpType=="MgFreeTest" & EPSC$Time==0 & EPSC$Select <=1,]))$coefficients[,2]),"\n"),cex=0.75,adj=c(0,0));
dev.off();
#-------------------Ploting AmpNARatioDepol & AmpNARatioMgFree from MgFreeTest experiment
jpeg("~/DATA/ELS/Rplots/AmpNARatioMgFree-Depol_MgFreeTestAll-SelCells_CTRL+VEH.jpg",width=18,height=18,units="cm",bg="white",res=300);
par(mfrow=c(1,4));
bargraph.CI(res=AmpNARatioMgFree,Groups,data=EPSC[EPSC$ExpType=="MgFreeTest" & EPSC$Time==0,],col=c("white"),legend=T,xlab=c("AmpNARatioMgFree"),ylim=c(0,0.5),space=1,cex.leg=1.5,cex.names=1.5,cex.lab=1.5,main="All cells",cex.axis=2);
bargraph.CI(res=AmpNARatioDepol,Groups,data=EPSC[EPSC$ExpType=="MgFreeTest" & EPSC$Time==0,],col=c("white"),legend=T,xlab=c("AmpNARatioDepol"),ylim=c(0,0.5),space=1,cex.leg=1.5,cex.names=1.5,cex.lab=1.5,main="All cells",cex.axis=2,axes=F);
#Selected cells
bargraph.CI(res=AmpNARatioMgFree,Groups,data=EPSC[EPSC$ExpType=="MgFreeTest" & EPSC$Time==0 & EPSC$Select==1,],col=c("white"),legend=T,xlab=c("AmpNARatioMgFree"),ylim=c(0,0.5),space=1,cex.leg=1.5,cex.names=1.5,cex.lab=1.5,main="Selected cells",cex.axis=2);
bargraph.CI(res=AmpNARatioDepol,Groups,data=EPSC[EPSC$ExpType=="MgFreeTest" & EPSC$Time==0 & EPSC$Select==1,],col=c("white"),legend=T,xlab=c("AmpNARatioDepol"),ylim=c(0,0.5),space=1,cex.leg=1.5,cex.names=1.5,cex.lab=1.5,main="Selected cells",cex.axis=2,axes=F);
dev.off();
#------------Ploting AmpNARatioMgFree from MgFree exp - All cells & Selected cells
jpeg("~/DATA/ELS/Rplots/AmpNARatioMgFree_MgFreeAll-SelCells.jpg",width=18,height=18,units="cm",bg="white",res=300);
par(mfrow=c(1,2));
bargraph.CI(Groups,res=AmpNARatioMgFree,Groups,data=EPSC[EPSC$ExpType=="MgFree" & EPSC$Time==0,],col=c("white","grey","lightblue","blue"),legend=F,xlab=c("AmpNARatioMgFree"),ylim=c(0,0.5),space=0.1,cex.leg=1,cex.names=1,cex.lab=1,main="All cells",cex.axis=2,x.leg=0,y.leg=0.5);
bargraph.CI(Groups,res=AmpNARatioMgFree,Groups,data=EPSC[EPSC$ExpType=="MgFree" & EPSC$Time==0 & EPSC$Select==1,],col=c("white","grey","lightblue","blue"),legend=T,xlab=c("AmpNARatioMgFree"),ylim=c(0,0.5),space=0.1,cex.leg=1,cex.names=1,cex.lab=1,main="Selected cells",cex.axis=2,axes=F,x.leg=0,y.leg=0.5);
dev.off();
#--------- Ploting AmpNARatioDepol from Depol exp - All cells $ Selected cells
jpeg("~/DATA/ELS/Rplots/AmpNARatioDepol_DepolAll-SelCells.jpg",width=18,height=18,units="cm",bg="white",res=300);
par(mfrow=c(1,2));
bargraph.CI(Groups,res=AmpNARatioDepol,Groups,data=EPSC[EPSC$ExpType=="Depol" & EPSC$Time==0,],col=c("white","grey","lightblue","blue"),legend=F,xlab=c("AmpNARatioDepol"),ylim=c(0,0.5),space=0.1,cex.leg=1,cex.names=1,cex.lab=1,main="All cells",cex.axis=2,x.leg=0,y.leg=0.5);
bargraph.CI(Groups,res=AmpNARatioDepol,Groups,data=EPSC[EPSC$ExpType=="Depol" & EPSC$Time==0 & EPSC$Select==1,],col=c("white","grey","lightblue","blue"),legend=T,xlab=c("AmpNARatioDepol"),ylim=c(0,0.5),space=0.1,cex.leg=1,cex.names=1,cex.lab=1,main="Selected cells",cex.axis=2,axes=F,x.leg=0,y.leg=0.5);
dev.off();
#-------------------Ploting AmpNARatioDepol & AmpNARatioMgFree from CTRL+VEH group of MgFree & Depol experiments With Conditions
jpeg("~/DATA/ELS/Rplots/AmpNARatio-CTRL+VEH~ExpType_SelWithCond.jpg",width=18,height=18,units="cm",bg="white",res=300);
par(mfrow=c(1,4));
bargraph.CI(res=AmpNARatioMgFree,Groups,data=EPSC[EPSC$ExpType=="MgFreeTest" & EPSC$Time==0 & EPSC$SelectOutlier==1 & EPSC$SelectCond==1,],col=c("white"),legend=T,xlab=c("AmpNARatioMgFree"),ylim=c(0,0.5),space=1,cex.leg=1.5,cex.names=1.5,cex.lab=1.5,main="MgFreeTestWithCond",cex.axis=2,cex.main=1.3);
bargraph.CI(res=AmpNARatioDepol,Groups,data=EPSC[EPSC$ExpType=="MgFreeTest" & EPSC$Time==0 & EPSC$SelectOutlier==1 & EPSC$SelectCond==1,],col=c("white"),legend=T,xlab=c("AmpNARatioDepol"),ylim=c(0,0.5),space=1,cex.leg=1.5,cex.names=1.5,cex.lab=1.5,main="MgFreeTestWithCond ",cex.axis=2,axes=F,cex.main=1.3);
bargraph.CI(res=AmpNARatioMgFree,Groups,data=EPSC[EPSC$ExpType=="MgFree" & EPSC$Time==0 & EPSC$Groups=="CTRL+VEH" & EPSC$SelectOutlier==1 & EPSC$SelectCond==1,],col=c("white"),legend=T,xlab=c("AmpNARatioMgFree"),ylim=c(0,0.5),space=1,cex.leg=1.5,cex.names=1.5,cex.lab=1.5,main="MgFreeWithCond",cex.axis=2,axes=T,cex.main=1.3);
bargraph.CI(res=AmpNARatioDepol,Groups,data=EPSC[EPSC$ExpType=="Depol" & EPSC$Time==0 & EPSC$Groups=="CTRL+VEH" & EPSC$SelectOutlier==1 & EPSC$SelectCond==1,],col=c("white"),legend=T,xlab=c("AmpNARatioDepol"),ylim=c(0,0.5),space=1,cex.leg=1.5,cex.names=1.5,cex.lab=1.5,main="DepolWithCond",cex.axis=2,axes=F,cex.main=1.3);
dev.off();
#-----------Ploting AmpNARatioMgFree ~ ExpType+Groups from combined experiments of selected cells + conditions
jpeg("~/DATA/ELS/Rplots/AmpNARatioMgFree~ExpType+Groups_FitSelCellsWithCond.jpg",width=18,height=18,units="cm",bg="white",res=300);
bargraph.CI(ExpType,AmpNARatioMgFree,Groups,data=EPSC[EPSC$BrainRegion=="CA1" & EPSC$Time==0 & EPSC$SelectOutlier==1 & EPSC$SelectCond==1,],col=c("white","grey","lightblue","blue"),legend=T,ylab=c("AmpNARatioMgFree"),ylim=c(0,0.5),cex.leg=1,cex.names=1.5,cex.lab=1.5,main="AmpNARatioMgFree all exps (Fit: selected cells + conditions )",cex.axis=1.5);
dev.off();
#-----------Ploting AmpNARatioMgFree ~ Groups from combined experiments of selected cells + conditions
jpeg("~/DATA/ELS/Rplots/AmpNARatioMgFree~Groups_SelCellsCondBLA.jpg",width=15,height=18,units="cm",bg="white",res=300);
#par(mfrow=c(1,2));
bargraph.CI(Groups,AmpNARatioMgFree,Groups,data=EPSC[EPSC$BrainRegion=="BLA" & EPSC$Time==0 & EPSC$SelectOutlier==1 & EPSC$SelectCond==1,],col=c("white","grey","lightblue","blue"),legend=T,ylab=c("AmpNARatioMgFree"),xlab=paste("BLA cells","\n","Cond: 55 < Age ∞ & -15 <= Reversal Potential <= 15"),ylim=c(0,0.5),cex.leg=1,cex.names=1.3,cex.lab=1.3,main="",cex.axis=1.5,space=0.1,x.leg=0,y.leg=0.5,axisnames=F);
dev.off();
#-----------Ploting Param ~ Groups from combined experiments of selected cells + conditions
jpeg("~/DATA/ELS/Rplots/Age~Groups_SelCellsCond.jpg",width=9,height=18,units="cm",bg="white",res=300);
#par(mfrow=c(1,2));
bargraph.CI(Groups,AvgTauFull,Groups,data=EPSC[EPSC$TimeIndex==1 & EPSC$SelectOutlier==1 & EPSC$SelectCond==1,],col=c("white","grey","lightblue","blue"),legend=T,ylab=c("Age"),xlab="Groups",cex.leg=1,cex.names=1.2,cex.lab=1.2,main="",cex.main=1,cex.axis=1.5,space=0.1,x.leg=0,y.leg=0.5);
dev.off();
#-----------Ploting RevPeakCor ~ Groups from combined experiments of selected cells + conditions
jpeg("~/DATA/ELS/Rplots/RevPeakCor~Groups_SelDepolCellsCond.jpg",width=9,height=18,units="cm",bg="white",res=300);
#par(mfrow=c(1,2));
bargraph.CI(Groups,RevPeakCor,Groups,data=EPSC[EPSC$Time==0 & EPSC$Select==1 & EPSC$ExpType=="Depol" & EPSC$Age>55 & EPSC$RevPeakCor<=15 & EPSC$RevPeakCor>=-15,],col=c("white","grey","lightblue","blue"),legend=T,ylab=c("Rev Peak Cor"),xlab="Only Depol cells",ylim=c(-10,10),cex.leg=1,cex.names=1.2,cex.lab=1.2,main="Age > 55   &   15 > RevPeakCor > -15",cex.main=1,cex.axis=1.5,space=0.1,x.leg=0,y.leg=10);
dev.off();
#----------Plotting IV-relationship for EPSCs - only selected cells + conditions
jpeg("~/DATA/ELS/Rplots/Tail~VClampA_SelDepolCellsCond.jpg",width=15,height=15,units="cm",bg="white",res=300);
lineplot.CI(VClampA,Peak,Groups,data=EPSC[EPSC$RecMode=="EPSCIV" & EPSC$SelectOutlier==1 & EPSC$Tail>-300 & EPSC$SelectCond==1,],col=c("black","grey","blue","purple"),legend=T,ylab=c("Tail EPSC amplitude (pA)"),main=paste("All selected cells + conds"),cex.leg=1,cex.names=1.3,cex.lab=1.3,xlab="Voltage step (mv)",cex.axis=1.5,space=0.1,x.leg=0.5,y.leg=40,axisnames=T,lwd=2);
abline(v=10,col="black",lwd=1,lty=2);
abline(h=0,col="black",lwd=1,lty=2);
dev.off();
#-----Plot the graphs of temporal summation : Paired pulse ratio :  PPF
par(mar=c(5,4,5,5));
jpeg("~/DATA/ELS/Rplots/PPFPeak4050~SelCellsCond_CA1.jpg",width=12,height=15,units="cm",bg="white",res=300);
bargraph.CI(GroupOrdered,PPFPeak,group=GroupOrdered,data=nSS[nSS$BrainRegion=="CA1" & nSS$VClamp == 40 & nSS$StimInterval==50 & nSS$StimNum==1 & nSS$Sweep==-1 & nSS$Select==1,],col=c("white","lightblue"),legend=F,ylab=c(" Paired pulse ratio"),space=0.05,cex.leg=1,cex.names=1.5,cex.lab=1.5,cex.axis=1.5,axes=T,x.leg=0,y.leg=0.5,ylim=c(0,3),main=paste("\n","55 days < Age < ∞","\n", "-15 <= Reversal Potential <= 15","\n","VClamp == 40 mV","\nStimInterval == 50 ms"),cex.main=1 );
dev.off();
#-----Plot the graphs of temporal summation : EPSC accommodation :  Acco
par(mar=c(5,4,5,5));
jpeg("~/DATA/ELS/Rplots/AccoPeak40100~SelCellsCond_CA1.jpg",width=12,height=15,units="cm",bg="white",res=300);
bargraph.CI(GroupOrdered,AccoPeak,group=GroupOrdered,data=nSS[nSS$BrainRegion=="CA1" & nSS$VClamp == 40 & nSS$StimInterval==100 & nSS$StimNum==1 & nSS$Sweep==-1 & nSS$Select==1,],col=c("white","grey","lightblue","blue"),legend=F,ylab=c(" Accommodation ratio"),space=0.05,cex.leg=1,cex.names=1.5,cex.lab=1.5,cex.axis=1.5,axes=T,x.leg=0,y.leg=0.5,ylim=c(0,2),main=paste("\n","55 days < Age < ∞","\n", "-15 <= Reversal Potential <= 15","\n","VClamp == 40 mV","\nStimInterval == 100 ms"),cex.main=1 );
dev.off();
#-----Plot the graphs of temporal summation : EPSC accommodation :  All stimulations
par(mar=c(5,4,5,5));
jpeg("~/DATA/ELS/Rplots/AccoPeak40100~SelCellsCond_CA1.jpg",width=12,height=15,units="cm",bg="white",res=300);
lineplot.CI(StimNum,NTail,group=GroupOrdered,data=nSS[nSS$BrainRegion=="CA1" & nSS$VClamp == 40 & nSS$StimInterval==100  & nSS$Sweep==-1 & nSS$Select==1,],col=c("white","grey","lightblue","blue"),legend=F,ylab=c(" Accommodation ratio"),space=0.05,cex.leg=1,cex.names=1.5,cex.lab=1.5,cex.axis=1.5,axes=T,x.leg=0,y.leg=0.5,ylim=c(0,3),main=paste("\n","55 days < Age < ∞","\n", "-15 <= Reversal Potential <= 15","\n","VClamp == 40 mV","\nStimInterval == 100 ms"),cex.main=1 );
dev.off();
#-----Plot the time course graphs of EPSC short-term potentiation
par(mar=c(5,4,5,5));
par(mgp=c(2.5,1,0);
jpeg("~/DATA/ELS/Rplots/NormPeak~Time_CORTgroupsSelCellsCond_CA1.jpg",width=18,height=15,units="cm",bg="white",res=300);
lineplot.CI(floor(Time),NormPeak,group=Groups,data=EPSC[EPSC$BrainRegion=="CA1" & EPSC$ExpType=="Depol" & EPSC$SelectOutlier==1 & EPSC$Exvivo=="CORT" & EPSC$SelectCond==1 & EPSC$Time>=-8 & EPSC$Time<=15,],axes=T,xlab="Time (min)",ylab="Peak (% normalized)",cex.axis=1.4,cex.lab=1.5,cex=1.3,cex.leg=1.25,x.leg=0,y.leg=200,fixed=T)
dev.off()
#-----Plot bar graphs of averaged EPSC short-term potentiation
par(mar=c(5,4,5,5));
par(mgp=c(2.5,1,0);
jpeg("~/DATA/ELS/Rplots/LTPPeak~GroupsSelCellsCond_CA1.jpg",width=14,height=15,units="cm",bg="white",res=300);
bargraph.CI(Groups,LTPPeak,group=Groups,data=EPSC[EPSC$BrainRegion=="CA1" & EPSC$ExpType == "Depol" & EPSC$Time==0 & EPSC$SelectOutlier==1,],col=c("white","grey","lightblue","blue"),legend=F,ylab=c("Peak (% normalized)"),space=0.05,cex.leg=1,cex.names=1.2,cex.lab=1.5,cex.axis=1.5,axes=T,x.leg=0,y.leg=0.5,ylim=c(0,200),main=paste(" CA1","\n", "","\n","Average = last 3 (13-15) mins","\n"),cex.main=1 );
abline(h=100,col="black",lwd=1,lty=2);
#text(testplot,par("usr")[1],labels=levels(EPSC$Groups),srt=45,cex=0.9);
#axis(2);
dev.off();
