
# Sholl plot Dendritic length CA3 apical
par(mar=c(4,4,2,2) + 0.1)
jpeg("F:/DATA/PostDoc1_Marian/HRIRLR/Complete_Trace_files_HRIRLR/R_files/HRIRLR_CA3_apical_Sholl.jpeg",units="in",res=600,height=4,width=5,pointsize=12)
newline(x.factor=as.factor(ca3apicalsholl[,"sholl"]),pin=c(4,5),response=dl,group=group,data=ca3apicalsholl,cex=1.3,legend=0,pch=c(21,21,21),bordercol=c("black","black","black"),fillcol=c("black","grey","white"),err.col=c("black"),cex.axis=1.3,x.cont=T,xlim=c(50,400),ylim=c(0,100),xlab="",ylab="",xaxt="n",yaxt="n",lwd=1.8,bty="n",lty=c(1,1,1))
axis(side=2, at=c(-5,0,25,50,75,100),pos=36,cex.axis=1.2,adj=0,lwd=2,las=2)
axis(side=1, at=c(20,50,100,150,200,250,300,350,400),pos=-4,cex.axis=1.2,lwd=2)
dev.off()

#Bar Graph Dendritic complexity-index
x11(h=5,3.5)
#jpeg("F:/DATA/PostDoc1_Marian/HRIRLR/Complete_Trace_files_HRIRLR/R_files/HRIRLR_CA3_dci_apical.jpeg",units="in",res=600,height=5,width=3.5,pointsize=13)
par(mar=c(3,6.5,3,0.8) + 0.1)
par(lwd=2)
bargraph.CI(col=c("black","grey","white"),yaxt="n",cex.axis=1.3,lwd=c(5,5,5),ylim=c(0,100000),x.factor=as.factor(ca3apicaltotal[,"group"]),response=t_dci,data=ca3apicaltotal)
axis(side=2, at=c(0,25000,50000,75000,100000),pos=0,cex.axis=1.6,adj=0,lwd=2,labels= c("0","25","50","75",expression(100%*%10^3)),las=2)
#dev.off()

#Bar Graph Branching Points CA3 apical
x11(h=5,3.5)
jpeg("F:/DATA/PostDoc1_Marian/HRIRLR/Complete_Trace_files_HRIRLR/R_files/HRIRLR_CA3_bp_apical.jpeg",units="in",res=600,height=5,width=3.5,pointsize=13)
par(mar=c(3,6.5,3,0.8) + 0.1)
par(lwd=2)
bargraph.CI(col=c("black","grey","white"),yaxt="n",cex.axis=1.3,lwd=c(5,5,5),ylim=c(0,15),x.factor=as.factor(ca3apicaltotal[,"group"]),response=t_bp,data=ca3apicaltotal)
axis(side=2, at=c(0,5,10,15),pos=0,cex.axis=1.6,adj=0,lwd=2,las=2)
dev.off()


# Sholl plot Dendritic length CA3 basal
x11(h=4,5)
par(mar=c(4,4,3,2) + 0.1)
jpeg("F:/DATA/PostDoc1_Marian/HRIRLR/Complete_Trace_files_HRIRLR/R_files/HRIRLR_CA3_basal_Sholl.jpeg",units="in",res=600,height=4,width=5,pointsize=12)
newline(lwd=2,x.factor=as.factor(ca3basalsholl2[,"sholl"]),pin=c(4,5),response=dl,group=group,data=ca3basalsholl2,cex=1.3,legend=0,pch=c(21,21,21),bordercol=c("black","black","black"),fillcol=c("black","grey","white"),err.col=c("black"),cex.axis=1.3,x.cont=T,xlim=c(35,250),ylim=c(0,125),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",lty=c(1,1,1))
axis(side=1,at=c(25,50,100,150,200,250),pos=-5,cex.axis=1.2,lwd=2,adj=0,labels= c("35","50","100","150","200","250"))
axis(side=2,at=c(-7,0,25,50,75,100,125),pos=26,cex.axis=1.2,lwd=2,las=2,yaxp=c(0,125,5),yaxs="i",adj=0)
dev.off()

#Bar Graph Dendritic complexity-index CA3 basal
x11(h=5,3.5)
jpeg("F:/DATA/PostDoc1_Marian/HRIRLR/Complete_Trace_files_HRIRLR/R_files/HRIRLR_CA3_dci_basal.jpeg",units="in",res=600,height=5,width=3.5,pointsize=13)
par(mar=c(3,6.5,3,0.8) + 0.1)
par(lwd=2)
bargraph.CI(col=c("black","grey","white"),yaxt="n",cex.axis=1.3,lwd=c(5,5,5),ylim=c(0,25000),x.factor=as.factor(tca3basal[,"group"]),response=t_dci,data=tca3basal)
axis(side=2, at=c(0,5000,10000,15000,20000,25000),pos=0,cex.axis=1.6,adj=0,lwd=2,labels= c("0","5","10","15","20",expression(25%*%10^3)),las=2)
dev.off()

#Bar Graph Branching Points CA3 basal
x11(h=5,3.5)
jpeg("F:/DATA/PostDoc1_Marian/HRIRLR/Complete_Trace_files_HRIRLR/R_files/HRIRLR_CA3_bp_basal.jpeg",units="in",res=600,height=5,width=3.5,pointsize=13)
par(mar=c(3,6.5,3,0.8) + 0.1)
par(lwd=2)
bargraph.CI(col=c("black","grey","white"),yaxt="n",cex.axis=1.3,lwd=c(5,5,5),ylim=c(0,15),x.factor=as.factor(tca3basal[,"group"]),response=t_bp,data=tca3basal)
axis(side=2, at=c(0,5,10,15),pos=0,cex.axis=1.6,adj=0,lwd=2,las=2)
dev.off()

#================================================================================

# Sholl plot Dendritic length CA1 apical
x11(h=4,5)
par(mar=c(4,4,3,2) + 0.1)
jpeg("F:/DATA/PostDoc1_Marian/HRIRLR/Complete_Trace_files_HRIRLR/R_files/HRIRLR_CA1_apical_Sholl.jpeg",units="in",res=600,height=4,width=5,pointsize=12)
newline(x.factor=as.factor(shollca1apical3[,"sholl"]),pin=c(4,5),response=dl,group=group,data=shollca1apical3,cex=1.3,legend=0,pch=c(21,21,21),bordercol=c("black","black","black"),fillcol=c("black","grey","white"),err.col=c("black"),cex.axis=1.3,x.cont=T,xlim=c(35,350),ylim=c(0,75),xlab="",ylab="",xaxt="n",yaxt="n",lwd=2,bty="n",lty=c(1,1,1))
axis(side=2, at=c(-5,0,25,50,75),pos=23,cex.axis=1.2,adj=0,lwd=2,las=2)
axis(side=1, at=c(20,50,100,150,200,250,300,350),pos=-3,cex.axis=1.2,lwd=2)
dev.off()

#Bar Graph Dendritic complexity-index CA1 apical
x11(h=5,3.5)
jpeg("F:/DATA/PostDoc1_Marian/HRIRLR/Complete_Trace_files_HRIRLR/R_files/HRIRLR_CA1_dci_apical.jpeg",units="in",res=600,height=5,width=3.5,pointsize=13)
par(mar=c(3,6.5,3,0.8) + 0.1)
par(lwd=2)
bargraph.CI(col=c("black","grey","white"),yaxt="n",cex.axis=1.3,lwd=c(5,5,5),ylim=c(0,125000),x.factor=as.factor(tca1apical2[,"group"]),response=t_dci,data=tca1apical2)
axis(side=2, at=c(0,25000,50000,75000,100000,125000),pos=0,cex.axis=1.6,adj=0,lwd=2,labels= c("0","25","50","75","100",expression(125%*%10^3)),las=2)
dev.off()

#Bar Graph Branching Points CA1 apical
x11(h=5,3.5)
jpeg("F:/DATA/PostDoc1_Marian/HRIRLR/Complete_Trace_files_HRIRLR/R_files/HRIRLR_CA1_bp_apical.jpeg",units="in",res=600,height=5,width=3.5,pointsize=13)
par(mar=c(3,6.5,3,0.8) + 0.1)
par(lwd=2)
bargraph.CI(col=c("black","grey","white"),yaxt="n",cex.axis=1.3,lwd=c(5,5,5),ylim=c(0,15),x.factor=as.factor(tca1apical2[,"group"]),response=t_bp,data=tca1apical2)
axis(side=2, at=c(0,5,10,15),pos=0,cex.axis=1.6,adj=0,lwd=2,las=2)
dev.off()

# Sholl plot Dendritic length CA1 basal
x11(h=4,5)
par(mar=c(4,4,3,2) + 0.1)
jpeg("F:/DATA/PostDoc1_Marian/HRIRLR/Complete_Trace_files_HRIRLR/R_files/HRIRLR_CA1_basal_Sholl.jpeg",units="in",res=600,height=4,width=5,pointsize=12)
newline(x.factor=as.factor(shollca1basal[,"sholl"]),pin=c(4,5),response=dl,group=group,data=shollca1basal,cex=1.3,legend=0,pch=c(21,21,21),bordercol=c("black","black","black"),fillcol=c("black","grey","white"),err.col=c("black"),cex.axis=1.3,x.cont=T,xlim=c(30,200),ylim=c(0,100),xlab="",ylab="",xaxt="n",yaxt="n",lwd=2,bty="n",lty=c(1,1,1))
axis(side=2, at=c(-7,0,25,50,75,100),pos=23,cex.axis=1.2,adj=0,lwd=2,las=2)
axis(side=1, at=c(22,50,100,150,200),pos=-4,cex.axis=1.2,lwd=2)
dev.off()
















