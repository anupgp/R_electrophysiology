## ======================================
## ELS experiment morphology Sholl plots
## ======================================
bargraph.CI(x.factor = group, response = dl, group = group, data = morallt[morallt$area=="ca1" & morallt$side=="basal",],ylim=c(0,2500));
morsca1a = morall[morall$side=="apical" & morall$area == "ca1",]; #morphology sholl ca1 apical
morsca1b = morall[morall$side=="basal" & morall$area == "ca1",]; #morphology sholl ca1 basal
morsbla = morall[morall$side=="full" & morall$area == "bla",]; #morphology sholl bla full
## average across groups/radius
morsavgca1a=ddply(morsca1a,c("group","radius"),function(x){df_temp=data.frame("group"=unique(x$group),"radius"=unique(x$radius),"dl.avg"=mean(x$dl,na.rm=TRUE),"dl.sem"=se(x$dl,na.rm=TRUE));return(df_temp);});
morsavgca1b=ddply(morsca1b,c("group","radius"),function(x){df_temp=data.frame("group"=unique(x$group),"radius"=unique(x$radius),"dl.avg"=mean(x$dl,na.rm=TRUE),"dl.sem"=se(x$dl,na.rm=TRUE));return(df_temp);});
morsavgbla=ddply(morsbla,c("group","radius"),function(x){df_temp=data.frame("group"=unique(x$group),"radius"=unique(x$radius),"dl.avg"=mean(x$dl,na.rm=TRUE),"dl.sem"=se(x$dl,na.rm=TRUE));return(df_temp);});
## ------------------------------------
jpeg("/mnt/storage/goofy/projects/data/els/figures/morsbla.jpeg",width=22,height=18,units="cm",bg="transparent",res=500);
par(bg="white",fg="black");par(omi = c(0.3,0.4,0.3,0.3));
xscale=seq(0,250,50);xscalelab=as.character(xscale);
yscale=seq(0,25,5);yscalelab=as.character(yscale);
## average points
plot(x=morsavgbla[morsavgbla[,"group"]=="control","radius"],y=morsavgbla[morsavgbla[,"group"]=="control",]$dl.avg,lwd=2,cex=3,cex.axis=2,axes=F,xlab="",ylab="",xaxt="n",yaxt="n",type="b",pch=21,xlim=c(0,250),ylim=c(0,25),col="black",bg="white");
points(x=morsavgbla[morsavgbla[,"group"]=="els","radius"],y=morsavgbla[morsavgbla[,"group"]=="els",]$dl.avg,lwd=2,cex=3,cex.axis=2,axes=F,xlab="",ylab="",xaxt="n",yaxt="n",pch=21,col="black",bg="black",type="b");
## error bars
segments(x0=morsavgbla[morsavgbla[,"group"]=="control","radius"], y0=morsavgbla[morsavgbla[,"group"]=="control","dl.avg"] - morsavgbla[morsavgbla[,"group"]=="control","dl.sem"],x1=morsavgbla[morsavgbla[,"group"]=="control","radius"], y1=morsavgbla[morsavgbla[,"group"]=="control","dl.avg"] + morsavgbla[morsavgbla[,"group"]=="control","dl.sem"],col="black",bg="black",lwd=2);
segments(x0=morsavgbla[morsavgbla[,"group"]=="els","radius"], y0=morsavgbla[morsavgbla[,"group"]=="els","dl.avg"] - morsavgbla[morsavgbla[,"group"]=="els","dl.sem"],x1=morsavgbla[morsavgbla[,"group"]=="els","radius"], y1=morsavgbla[morsavgbla[,"group"]=="els","dl.avg"] + morsavgbla[morsavgbla[,"group"]=="els","dl.sem"],col="black",lwd=2);
axis(1,at=xscale,labels=xscalelab,tick=T,lwd=3,cex.axis=3,xpd=F,las=1,padj=0.5);
axis(2,at=yscale,labels=yscalelab,tick=T,lwd=3,cex.axis=3,xpd=F,las=1);
dev.off();
## ------------------------------
## jpeg("/mnt/storage/goofy/projects/data/els/figures/morsca1b.jpeg",width=22,height=18,units="cm",bg="transparent",res=300);
jpeg("/mnt/storage/goofy/projects/data/els/figures/morsca1b.jpeg",width=22,height=18,units="cm",bg="transparent",res=300);
par(bg="white",fg="black");par(omi = c(0.3,0.3,0.3,0.3));
xscale=seq(0,250,50);xscalelab=as.character(xscale);
yscale=seq(0,-200,-50);yscalelab=c("0","50","100","150","200");
## extra average points
plot(x=morsavgca1b[morsavgca1b[,"group"]=="control","radius"],y=-morsavgca1b[morsavgca1b[,"group"]=="control",]$dl.avg,lwd=2,cex=3,cex.axis=2,axes=F,xlab="",ylab="",xaxt="n",yaxt="n",type="b",pch=21,xlim=c(0,250),ylim=c(-200,0),col="black",bg="white");
points(x=morsavgca1b[morsavgca1b[,"group"]=="els","radius"],y=-morsavgca1b[morsavgca1b[,"group"]=="els",]$dl.avg,lwd=2,cex=3,cex.axis=2,axes=F,xlab="",ylab="",xaxt="n",yaxt="n",pch=21,col="black",bg="black",type="b");
## extra error bars
segments(x0=morsavgca1b[morsavgca1b[,"group"]=="control","radius"], y0=-(morsavgca1b[morsavgca1b[,"group"]=="control","dl.avg"] - morsavgca1b[morsavgca1b[,"group"]=="control","dl.sem"]),x1=morsavgca1b[morsavgca1b[,"group"]=="control","radius"], y1=-(morsavgca1b[morsavgca1b[,"group"]=="control","dl.avg"] + morsavgca1b[morsavgca1b[,"group"]=="control","dl.sem"]),col="black",bg="black",lwd=2);
segments(x0=morsavgca1b[morsavgca1b[,"group"]=="els","radius"], y0=-(morsavgca1b[morsavgca1b[,"group"]=="els","dl.avg"] - morsavgca1b[morsavgca1b[,"group"]=="els","dl.sem"]),x1=morsavgca1b[morsavgca1b[,"group"]=="els","radius"], y1=-(morsavgca1b[morsavgca1b[,"group"]=="els","dl.avg"] + morsavgca1b[morsavgca1b[,"group"]=="els","dl.sem"]),col="black",lwd=2);
axis(2,at=yscale,labels=yscalelab,tick=T,lwd=3,cex.axis=3,xpd=F,las=1);
dev.off();
## ==========================================================
## ELS experiment morphology bar plot: total dendritic length 
## ==========================================================
mortca1a = morallt[morallt$side=="apical" & morallt$area == "ca1",]; #morphology sholl ca1 apical
mortca1b = morallt[morallt$side=="basal" & morallt$area == "ca1",]; #morphology sholl ca1 basal
mortbla = morallt[morallt$side=="full" & morallt$area == "bla",]; #morphology sholl bla full
## average across groups: total dendritic length
mortavgca1a=ddply(mortca1a,c("group"),function(x){df_temp=data.frame("group"=unique(x$group),"dl.avg"=mean(x$dl,na.rm=TRUE),"dl.sem"=se(x$dl,na.rm=TRUE));return(df_temp);});
mortavgca1b=ddply(mortca1b,c("group"),function(x){df_temp=data.frame("group"=unique(x$group),"dl.avg"=mean(x$dl,na.rm=TRUE),"dl.sem"=se(x$dl,na.rm=TRUE));return(df_temp);});
mortavgbla=ddply(mortbla,c("group"),function(x){df_temp=data.frame("group"=unique(x$group),"dl.avg"=mean(x$dl,na.rm=TRUE),"dl.sem"=se(x$dl,na.rm=TRUE));return(df_temp);});
bargraph.CI(x.factor = group, response = dl, group = group, data = mort,ylim=c(0,2500));
## ----------------------------------------------------------
mortavg=mortavgca1a;
jpeg("/mnt/storage/goofy/projects/data/els/figures/mortavgca1a.jpeg",width=10,height=18,units="cm",bg="transparent",res=300)
par(bg="white",fg="black",mar=c(5,5,4,2)+c(0,3.5,0,0),lwd=4);
barplotvals=c(mortavg[mortavg$group=="control","dl.avg"],mortavg[mortavg$group=="els","dl.avg"]);
## plot average 
barplot(barplotvals,col=c("white","black"),xlim=c(0,2.5),ylim=c(0,2500),axes=FALSE,fg="black");
## plot error bars
arrows(x0=0.7, y0=mortavg[mortavg$group=="control","dl.avg"] - mortavg[mortavg$group=="control","dl.sem"], x1=0.7, y1=mortavg[mortavg$group=="control","dl.avg"] + mortavg[mortavg$group=="control","dl.sem"],col="black",lwd=4,length=0.05,angle=90,code=3);
arrows(x0=1.9, y0=mortavg[mortavg$group=="els","dl.avg"], x1=1.9, y1=mortavg[mortavg$group=="els","dl.avg"] + mortavg[mortavg$group=="els","dl.sem"],col="black",lwd=4,length=0.05,angle=90,code=3);
arrows(x0=1.9, y0=mortavg[mortavg$group=="els","dl.avg"], x1=1.9, y1=mortavg[mortavg$group=="els","dl.avg"] - mortavg[mortavg$group=="els","dl.sem"],col="white",lwd=3,length=0.05,angle=90,code=3,bg="white");
axis(2,at=c(0,1250,2500),labels=as.character(c("0","1250","2500")),tick=T,lwd=3,cex.axis=3.5,xpd=F,las=1,padj=0.5);
dev.off();
## segments(x0=0.7, y0=(mortavg[mortavg$group=="control","dl.avg"] - mortavg[mortavg$group=="control","dl.sem"]),x1=0.7, y1=(mortavg[mortavg$group=="control","dl.avg"] + mortavg[mortavg$group=="control","dl.sem"]),col="black",lwd=4);
## ----------------------------------------------
jpeg("/mnt/storage/goofy/projects/data/els/figures/mortavgca1b.jpeg",width=10,height=18,units="cm",bg="transparent",res=300)
mortavg=mortavgca1b;
par(bg="white",fg="black",mar=c(5,5,4,2)+c(0,3.5,0,0),lwd=4);
barplotvals=c(-mortavg[mortavg$group=="control","dl.avg"],-mortavg[mortavg$group=="els","dl.avg"]);
## plot average 
barplot(barplotvals,col=c("white","black"),xlim=c(0,2.5),ylim=c(-2000,0),axes=FALSE,fg="black");
## plot error bars
arrows(x0=0.7, y0=-(mortavg[mortavg$group=="control","dl.avg"] - mortavg[mortavg$group=="control","dl.sem"]), x1=0.7, y1=-(mortavg[mortavg$group=="control","dl.avg"] + mortavg[mortavg$group=="control","dl.sem"]),col="black",lwd=4,length=0.05,angle=90,code=3);
arrows(x0=1.9, y0=-mortavg[mortavg$group=="els","dl.avg"], x1=1.9, y1=-(mortavg[mortavg$group=="els","dl.avg"] + mortavg[mortavg$group=="els","dl.sem"]),col="black",lwd=4,length=0.05,angle=90,code=3);
arrows(x0=1.9, y0=-mortavg[mortavg$group=="els","dl.avg"], x1=1.9, y1=-(mortavg[mortavg$group=="els","dl.avg"] - mortavg[mortavg$group=="els","dl.sem"]),col="white",lwd=3,length=0.05,angle=90,code=3,bg="white");
axis(2,at=c(0,-1000,-2000),labels=as.character(c("0","1000","2000")),tick=T,lwd=3,cex.axis=3.5,xpd=F,las=1,padj=0.5);
dev.off();
## -----------------------------------------------
## ----------------------------------------------
jpeg("/mnt/storage/goofy/projects/data/els/figures/mortavgbla.jpeg",width=10,height=18,units="cm",bg="transparent",res=300)
mortavg=mortavgbla;
par(bg="white",fg="black",mar=c(5,5,4,2)+c(0,3.5,0,0),lwd=4);
barplotvals=c(mortavg[mortavg$group=="control","dl.avg"],mortavg[mortavg$group=="els","dl.avg"]);
## plot average 
barplot(barplotvals,col=c("white","black"),xlim=c(0,2.5),ylim=c(0,300),axes=FALSE,fg="black");
## plot error bars
arrows(x0=0.7, y0=mortavg[mortavg$group=="control","dl.avg"] - mortavg[mortavg$group=="control","dl.sem"], x1=0.7, y1=mortavg[mortavg$group=="control","dl.avg"] + mortavg[mortavg$group=="control","dl.sem"],col="black",lwd=4,length=0.05,angle=90,code=3);
arrows(x0=1.9, y0=mortavg[mortavg$group=="els","dl.avg"], x1=1.9, y1=mortavg[mortavg$group=="els","dl.avg"] + mortavg[mortavg$group=="els","dl.sem"],col="black",lwd=4,length=0.05,angle=90,code=3);
arrows(x0=1.9, y0=mortavg[mortavg$group=="els","dl.avg"], x1=1.9, y1=mortavg[mortavg$group=="els","dl.avg"] - mortavg[mortavg$group=="els","dl.sem"],col="white",lwd=3,length=0.05,angle=90,code=3,bg="white");
axis(2,at=c(0,150,300),labels=as.character(c("0","150","300")),tick=T,lwd=3,cex.axis=3.5,xpd=F,las=1,padj=0.5);
dev.off();
## -----------------------------------------------
