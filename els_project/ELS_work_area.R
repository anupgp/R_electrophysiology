ls()
apvdf = MgFreeTest[MgFreeTest$CellID%in%apvcells,c("CellID","GroupName","ExpType","RecMode","TimeIndex","Time","Peak","NormPeak","AreaPeak","NormAreaPeak","TauPeak","RiseTime","Tail","NormTail","AreaTail","NormAreaTail","TauTail","SeriesRes","InputRes")];
apvdf[apvdf$TauPeak>0.1,"TauPeak"]=NA;apvdf[apvdf$TauTail>0.1,"TauTail"]=NA;
apvdf[apvdf$Time>9,"TauPeak"]=0;apvdf[apvdf$Time>9,"TauTail"]=0;
apvdflong = reshape(data=apvdf,direction="long",varying=c("Peak","Tail"),v.names="Val",timevar="Type")
apvdflong = reshape(data=apvdf,direction="long",varying=c("NormPeak","NormTail"),v.names="Val",timevar="Type")
apvdflong = reshape(data=apvdf,direction="long",varying=c("AreaPeak","AreaTail"),v.names="Val",timevar="Type")
apvdflong = reshape(data=apvdf,direction="long",varying=c("NormAreaPeak","NormAreaTail"),v.names="Val",timevar="Type")
apvdflong = reshape(data=apvdf,direction="long",varying=c("TauPeak","TauTail"),v.names="Val",timevar="Type")
apvdflong = reshape(data=apvdf,direction="long",varying=c("RiseTime"),v.names="Val",timevar="Type")
apvdflong = reshape(data=apvdf,direction="long",varying=c("SeriesRes","InputRes"),v.names="Val",timevar="Type")
tiff("~/DATA/ELS/Figures/MgFreeRepPeak.tiff",width=25,height=10,units="cm",compression=c("jpeg"),bg="white",res=300)
#plotdf = apvdflong[apvdflong$CellID%in%apvcells[4] & apvdflong$Time>=-5 & apvdflong$Type==1,];
#plotdf[25,"Val"]=0.08;# for plotting Tau
#plotdf[plotdf$Time>12 & plotdf$Val>0.05,"Val"] = 0.00;#for plotting Tau
#lineplot.CI(Time,Val*1000,Type,data=plotdf,type="p",legend=F,cex=1.5,ylim=c(-5,150),xaxt="n",yaxt="n",xlab="",ylab="",frame.plot=F);
#par(mar=c(0,0,0,0));#plots without margins
#clip=c(par("usr")[1],par("usr")[2],par("usr")[3],170);
par(bty="n");
par(cex.axis=2);
par(lwd=2);
par(las=2);
#par(col.axis="white");
par(mar=c(5,4.3,4,2));
gap.plot(x=plotdf$Time,y=plotdf$Val,gap.axis="y",gap=c(-130,-30),breakcol="white",type="p",cex=2,ylim=c(-250,1),xlab="",ylab="",xtics=c(-5,0,8,16),xticlab=c("","","",""),ytics=c(-250,-150,0),yticlab=c("","","0"),lwd=2);
#plot(x=plotdf$Time,y=plotdf$Val,type="p",cex=2,xlab="",ylab="",lwd=2,ylim=c(-250,5),axes=F,xlim=c(-5,16));#Peak
#plot(x=plotdf$Time,y=plotdf$Val*1000,type="p",cex=2,xlab="",ylab="",lwd=2,ylim=c(-0.25,25),axes=F,xlim=c(-5,16));#RT
#plot(x=plotdf$Time,y=plotdf$Val*1000,type="p",cex=2,xlab="",ylab="",lwd=2,ylim=c(-5,150),xlim=c(-5,16),axes=F);#Tau
#axis(1,labels=F);axis(2,labels=F);
axis(3,at=c(-5,0,8,16),labels=as.character(c(-5,0,8,16)),tick=T,lwd=2,cex.axis=2,xpd=F,las=1)
#axis(2,at=c(-250,-100,0),labels=as.character(c(-250,-100,0)),tick=T,cex.axis=2,xpd=F,lwd=2);#Rep Peak
axis(2,at=c(-250,-150,-130,0),labels=as.character(c(-250,-150,-30,0)),tick=T,cex.axis=2,xpd=F,lwd=2);#Rep Peak gap
axis.break(axis=2,breakpos=-130,style="slash");
#axis(4,at=c(50,25,0),labels=c("-50","","0"),tick=T,cex.axis=2,xpd=F,lwd=2);#Rep Peak gap
#axis(2,at=c(0,12,25),labels=as.character(c(0,12,25)),tick=T,cex.axis=2,xpd=F,lwd=2);#Rep RT
#axis(2,at=c(0,50,100,150),labels=as.character(c(0,50,100,150)),tick=T,cex.axis=2,xpd=F,lwd=2);#Rep Tau
#ablineclip(h=30,lty=3,lwd=2,x1=-5,x2=16);# Rep RT
ablineclip(h=-200,lty=3,lwd=2,x1=-5,x2=16);# Rep Peak
#ablineclip(h=3.5,lty=3,lwd=2,x1=-5,x2=16);# Rep RT
dev.off();
#plot.tc<-qplot(Time,Val, color=factor(Type),color=factor(Type),data=apvdflong[apvdflong$CellID%in%apvcells[3],],geom="point", position="dodge")+
	scale_colour_manual("Group",breaks = levels(factor(apvdflong$Type)), values= c("black","black") )+
	scale_fill_manual("Group",breaks = levels(factor(apvdflong$Type)), values= c("black","white") );
        plot.tc;
names(EPSC)
View(apvdflong)
names(apvdflong)
#---------------------------- 20140218 ---------------------------------
# The below commands are to plot the AmpNARatioMgFree from the 'datum' dataframe
png("~/DATA/ELS/Figures/AmpNARatioMgFree_BLA.png",width=12,height=18,units="cm",bg="transparent",res=300)
par(bty="n");
#par(btx="n");
par(cex.axis=2);
#par(lwd=2);
par(las=2);
#par(col.axis="white");
par(mar=c(4,6,4,2));
#(AmpNARatioMgFree ~ Groups,data=datum,legend=F,cex=1.5,ylim=c(0,0.5),xaxt="n",yaxt="n",xlab="",ylab="",frame.plot=F);
lineplot.CI(Groups,AmpNARatioMgFree,Groups,data=datum,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,0.5),xlim=c(0.6,5),legend=F,axes=F,lwd=2);
boxplot(AmpNARatioMgFree ~ Groups,data=datum,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
axis(1,at=c(1,2,3,4),labels=as.character(c("","","","")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
axis(2,at=c(0,0.25,0.5),labels=as.character(c("0","0.25","0.5")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
dev.off();
# The below commands are to plot the AvgAMPA from the 'datum' dataframe ---------------------------
png("~/DATA/ELS/Figures/AvgAMPA_BLA.png",width=12,height=18,units="cm",bg="transparent",res=300)
par(bty="n");
#par(btx="n");
par(cex.axis=2);
#par(lwd=2);
par(las=2);
#par(col.axis="white");
par(mar=c(4,6,4,2));
lineplot.CI(Groups,AvgAMPA,Groups,data=datum,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(-400,0),xlim=c(0.6,5),legend=F,axes=F,lwd=2);
boxplot(AvgAMPA ~ Groups,data=datum,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
axis(1,at=c(1,2,3,4),labels=as.character(c("","","","")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
axis(2,at=c(-400,-200,0),labels=as.character(c("-400","-200","0")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
dev.off();
# The below commands are to plot the AvgNMDA from the 'datum' dataframe ---------------------------
png("~/DATA/ELS/Figures/AvgNMDA_BLA.png",width=12,height=18,units="cm",bg="transparent",res=300)
par(bty="n");
#par(btx="n");
par(cex.axis=2);
#par(lwd=2);
par(las=2);
#par(col.axis="white");
par(mar=c(4,6,4,2));
lineplot.CI(Groups,AvgNMDA,Groups,data=datum,type="p",col="black",cex=3,err.lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(-65,0),xlim=c(0.6,5),legend=F,axes=F,lwd=2);
boxplot(AvgNMDA ~ Groups,data=datum,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
axis(1,at=c(1,2,3,4),labels=as.character(c("","","","")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
axis(2,at=c(-60,-30,0),labels=as.character(c("-60","-30","0")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1)
dev.off();
#-----------------------------20140220 -----------------------
boxplot(AvgRiseTime ~ Groups,data=datum,legend=F,add=T,boxwex=0.7,xlab="",staplewex=1,names=F,lwd=3,frame.plot=F,axes=F);
#-----------------------------20140224 ----------------------
