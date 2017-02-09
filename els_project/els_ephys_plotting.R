options("width"= 170);
source("/mnt/storage/goofy/projects/codes/els/R/els_ephys_preprocessing.R");
source("/mnt/storage/goofy/projects/codes/els/R/els_ephys_processing.R");
## ====================================================================
jpeg("/mnt/storage/goofy/projects/data/els/figures/ca1ss70_cort.jpeg",width=10,height=20,units="cm",bg="transparent",res=300)
## par(bg="white",fg="black",mar=c(5,5,4,2)+c(0,3.5,0,0),lwd=4);
par(mar=c(3,6,2.2,3));par(mgp=c(4,1,0));par(lwd = 3); #(bottom, left,top,right)
par(bg="white");
## -----------
plotdat=ca1ssdat;
plotvar="PPFPeak";
## ---------
avgdat=ddply(plotdat,c("GroupName"),function(x){dftemp=data.frame("GroupName"=unique(x$GroupName),"avg"=mean(x[,plotvar],na.rm=TRUE),"sem"=se(x[,plotvar],na.rm=TRUE));return(dftemp);});
plotavg=c(avgdat[avgdat$GroupName=="CTRL+CORT","avg"],avgdat[avgdat$GroupName=="ELS+CORT","avg"]);
plotsem=c(avgdat[avgdat$GroupName=="CTRL+CORT","sem"],avgdat[avgdat$GroupName=="ELS+CORT","sem"]);
## plot average
## barplot(plotavg,col=c("white","grey"),xlim=c(0,2.5),ylim=c(0,3),axes=FALSE); # first call to fill the bars
barplot(plotavg,col=c("white","black"),xlim=c(0,2.5),ylim=c(0,3),axes=FALSE); # first call to fill the bars
barplot(plotavg,col=c("black"),xlim=c(0,2.5),ylim=c(0,3),axes=FALSE,density=c(10,0),angle=c(45,0),fg=c("black"),border=c("black"),add=TRUE); # 2'nd call to shade
## plot error bars
arrows(x0=0.7, y0=plotavg[1]-plotsem[1], x1=0.7, y1=plotavg[1]+ plotsem[1],col="black",lwd=4,length=0.05,angle=90,code=3);
## arrows(x0=1.9, y0=plotavg[2]-plotsem[2], x1=1.9, y1=plotavg[2]+ plotsem[2],col="black",lwd=4,length=0.05,angle=90,code=3);
arrows(x0=1.9, y0=plotavg[2], x1=1.9, y1=plotavg[2]+plotsem[2],col="black",lwd=4,length=0.05,angle=90,code=3);
arrows(x0=1.9, y0=plotavg[2], x1=1.9, y1=plotavg[2] - plotsem[2],col="white",lwd=3,length=0.05,angle=90,code=3,bg="white");
axis(2,at=c(0,1,2,3),labels=as.character(c("0","1","2","3")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1,padj=0.5);
dev.off();

## ====================================================================
## Plot NA ratio data
par(mar=c(4,6.5,5,5));par(mgp=c(4,1,0));par(lwd = 2);
bargraph.CI(x.factor = GroupOrd, response = narAMmgfree , group = GroupOrd, data = datum, space = 0.1, cex.names = 0.65, cex.lab = 2, ylim = c(0,0.5), main = "CA1", cex.axis = 3, cex.main = 2, lwd = 2, err.lty = 1, err.col = "black", font.axis=1, family="sans", font=1, font.lab=1, density = c(0,100,10,100), angle = c(0,-45,45,45), col = c("black","grey","black","black"), legend = F, x.leg =  0, y.leg = 0.5, cex.leg=2, ylab=" Paired pulse ratio (EPSC2 / EPSC1)",yaxp=c(0,1,4));
#abline(h = 0.263, col = "grey", lwd = 0.1, lty = 2);

## Plot PPF data
par(mar=c(4,6.5,5,5));par(mgp=c(4,1,0));par(lwd = 2);
bargraph.CI(x.factor = GroupOrdered, response = PPFPeak , group = GroupOrdered, fixed = T, data = datum, space = 0.1, cex.leg = 1.3, cex.names = 0.65, cex.lab = 2, ylim = c(0,2.5), main = "CA1", cex.axis = 2, cex.main = 2, lwd = 2, err.lty = 1, err.col = "black", density = c(0,100,10,100), angle = c(0,-45,45,45), col = c("black","grey","black","black"), legend = F,ylab=" Paired Pulse facilitation ");
abline(h = 1, col = "grey", lwd = 0.1, lty = 2);

## Plot PPF data
par(mar=c(4,6.5,5,5));par(mgp=c(4,1,0));par(lwd = 2);
bargraph.CI(x.factor = GroupOrdered, response = PPFPeak , group = GroupOrdered, data = datum, space = 0.1, cex.names = 0.65, cex.lab = 2, ylim = c(0,3), main = "CA1", cex.axis = 3, cex.main = 2, lwd = 2, err.lty = 1, err.col = "black", font.axis=1, family="sans", font=1, font.lab=1, density = c(0,100,10,100), angle = c(0,-45,45,45), col = c("black","grey","black","black"), legend = F, x.leg =  0, y.leg = 0.5, cex.leg=2, ylab=" Paired pulse ratio (EPSC2 / EPSC1)",yaxp=c(0,3,3));
#abline(h = 0.263, col = "grey", lwd = 0.1, lty = 2);
## --------------------
scatterplot(PPFPeak ~ GroupOrdered, data=datum);
boxplot(narAMmgfree ~ GroupOrd, data = datum);
jpeg("/mnt/storage/goofy/projects/els/figures/bla_naratio.jpg",width=18,height=25,units = "cm",bg="white",res=100);
dev.off()
## plotting nSS -----------------
png("~/DATA/ELS/Figures/AccoPeak40100.png",width=12,height=18,units="cm",bg="transparent",res=300)
par(bty="n");##par(btx="n");par(cex.axis=2);##par(lwd=2);par(las=2);##par(col.axis="white");
par(mar=c(4,6,4,2));
lineplot.CI(GroupOrdered,AccoPeak,GroupOrdered,data=ssdatum,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,3),xlim=c(0.6,5),legend=F,axes=F,lwd=2);
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
axis(1,at=c(1,2,3,4),labels=as.character(c("","","","")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
axis(2,at=c(0,1,2,3),labels=as.character(c("0","1","2","3")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
ablineclip(h=1,lty=3,lwd=2,x1=0,x2=4);## Rep Peak
dev.off();
## plotting nSS -----------------
png("~/DATA/ELS/Figures/AccoPeak4050.png",width=12,height=18,units="cm",bg="transparent",res=300)
par(bty="n");##par(btx="n");par(cex.axis=2);##par(lwd=2);par(las=2);##par(col.axis="white");
par(mar=c(4,6,4,2));
lineplot.CI(GroupOrdered,AccoPeak,GroupOrdered,data=ssdatum,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,5),xlim=c(0.6,5),legend=F,axes=F,lwd=2);
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
axis(1,at=c(1,2,3,4),labels=as.character(c("","","","")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
axis(2,at=c(0,2.5,5),labels=as.character(c("1","2.5","5")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
ablineclip(h=1,lty=3,lwd=2,x1=0,x2=4);## Rep Peak
dev.off();
## plotting nSS -----------------
png("~/DATA/ELS/Figures/AccoPeak7050.png",width=12,height=18,units="cm",bg="transparent",res=300)
par(bty="n");##par(btx="n");par(cex.axis=2);##par(lwd=2);par(las=2);##par(col.axis="white");
par(mar=c(4,6,4,2));
lineplot.CI(GroupOrdered,AccoPeak,GroupOrdered,data=ssdatum,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0.5,3.5),xlim=c(0.6,5),legend=F,axes=F,lwd=2);
boxplot(AccoPeak ~ GroupOrdered,data=ssdatum,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
axis(1,at=c(1,2,3,4),labels=as.character(c("","","","")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
axis(2,at=c(0.5,1.5,2.5,3.5),labels=as.character(c("0.5","1.5","2.5","3.5")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
ablineclip(h=1,lty=3,lwd=2,x1=0,x2=4);## Rep Peak
dev.off();
## plotting LTPPeak  -----------------
png("~/DATA/ELS/Figures/DepolLTPPeak_CA1.png",width=12,height=18,units="cm",bg="transparent",res=300)
par(bty="n");par(cex.axis=2);par(las=2);par(mar=c(4,6,4,2));
lineplot.CI(Groups,LTPPeak,GroupName,data=datumltp,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,200),xlim=c(0.6,5),legend=F,axes=F,lwd=2);
boxplot(LTPPeak ~ Groups,data=datumltp,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
axis(1,at=c(1,2,3,4),labels=as.character(c("","","","")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
axis(2,at=c(0,50,100,150,200),labels=as.character(c("0","50","100","150","200")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
ablineclip(h=100,lty=3,lwd=2,x1=0,x2=4);## Rep Peak
dev.off();
##---------------------------
par(bty="n");
##par(btx="n");
par(cex.axis=2);
##par(lwd=2);
par(las=2);
##par(col.axis="white");
## -----------------------
tcltp=EPSC[EPSC$BrainRegion=="CA1" & (EPSC$ExpType=="Depol") & EPSC$SelectDepolH <=1 & EPSC$SelectAge ==1 & EPSC$RevPeakCor >=-30 & EPSC$RevPeakCor<=30 & (!EPSC$CellID%in%chuckdepol) & (!EPSC$CellID%in%boxdepol2)& (!EPSC$CellID%in%boxall),]## & (!EPSC$CellID%in%boxltp),];##For LTP Time Course
png("~/DATA/ELS/Figures/Depol_TC_Invivo-CTRL_CA1.png",width=22,height=18,units="cm",bg="transparent",res=300)
par(bty="n");par(cex.axis=2);par(las=2);par(mar=c(4,6,4,2));par(btx="n");
lineplot.CI(Time,NormPeak,GroupName,data=tcltp,,subset=Invivo=="CTRL" & Time>=-5 & Time<=15,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(-100,200),xlim=c(0,49),legend=F,axes=F,lwd=2);
axis(1,at=c(0,12,24,36,48),labels=as.character(c("-5","0","5","10","15")),tick=T,lwd=3,cex.axis=3,xpd=T,las=1,padj=0.6,hadj=0.6)
axis(2,at=c(-100,-50,0,50,100,175),labels=as.character(c("-100","","0","","100","175")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
ablineclip(h=100,lty=3,lwd=2,x1=0,x2=48,col="black");## Rep Peak
dev.off();
