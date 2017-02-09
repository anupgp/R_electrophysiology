# Codes to plot the x-y error bar for AHP - asked by reviewers
Fxyerror <-  function(df,cid,xvar,yvar,wvar,bvar)
   {
   #------xmean
   xmean = tapply(df[,xvar],list(df[,wvar],df[,bvar]),FUN=mean);
   xmean = cbind(xmean,withinvar=rownames(xmean));
   colnames(xmean)=sub(x=colnames(xmean),rep=wvar,pat="withinvar");
   xmean = as.data.frame(xmean,row.names=NULL,stringsAsFactors=F);
   xmean = reshape(xmean,dir="long",varying=as.character(unique(df[,bvar])),v.names=xvar,idvar=wvar,times=unique(df[,bvar]),timevar=bvar);
   #------xsem
   xsem = tapply(df[,xvar],list(df[,wvar],df[,bvar]),FUN=se);
   xsem = cbind(xsem,withinvar=rownames(xsem));
   colnames(xsem)=sub(x=colnames(xsem),rep=wvar,pat="withinvar");
   xsem = as.data.frame(xsem,row.names=NULL,stringsAsFactors=F);
   xsem = reshape(xsem,dir="long",varying=as.character(unique(df[,bvar])),v.names=xvar,idvar=wvar,times=unique(df[,bvar]),timevar=bvar);
   #------ymean
   ymean = tapply(df[,yvar],list(df[,wvar],df[,bvar]),FUN=mean);
   ymean = cbind(ymean,withinvar=rownames(ymean));
   colnames(ymean)=sub(x=colnames(ymean),rep=wvar,pat="withinvar");
   ymean = as.data.frame(ymean,row.names=NULL,stringsAsFactors=F);
   ymean = reshape(ymean,dir="long",varying=as.character(unique(df[,bvar])),v.names=yvar,idvar=wvar,times=unique(df[,bvar]),timevar=bvar);
   #------ysem
   ysem = tapply(df[,yvar],list(df[,wvar],df[,bvar]),FUN=se);
   ysem = cbind(ysem,withinvar=rownames(ysem));
   colnames(ysem)=sub(x=colnames(ysem),rep=wvar,pat="withinvar");
   ysem = as.data.frame(ysem,row.names=NULL,stringsAsFactors=F);
   ysem = reshape(ysem,dir="long",varying=as.character(unique(df[,bvar])),v.names=yvar,idvar=wvar,times=unique(df[,bvar]),timevar=bvar);
   #------combine/merge
   xymean = merge(xmean,ymean,by=c(bvar,wvar));
   xymean = ddply(xymean,c(bvar),FUN=function(x){data.frame(x[order(x[,xvar]),],stringsAsFactors=F)});
   xysem = merge(xsem,ysem,by=c(bvar,wvar));
   xysem = ddply(xysem,c(bvar),FUN=function(x){data.frame(x[order(x[,xvar]),],stringsAsFactors=F)});
   #------plotting
   xymean[,xvar] = as.numeric(xymean[,xvar]);
   xymean[,yvar] = as.numeric(xymean[,yvar]);
   xysem[,xvar] = as.numeric(xysem[,xvar]);
   xysem[,yvar] = as.numeric(xysem[,yvar]);
   groups = unique(xymean[,bvar])
   xylinecols = c("grey50","black");
   xylnwidth=2;
   xymarkercols = c("grey50","black");
   xysecols = xymarkercols;
   marker=c(17,19);# triangle, circle
   #xylimx = c(min(xymean[,xvar])-min(xysem[,xvar]),max(xymean[,xvar])+max(xysem[,xvar]));
   #xylimy = round(c(min(xymean[,yvar])-max(xysem[,yvar]),max(xymean[,yvar])+max(xysem[,yvar])));
   xylimx = c(4,23);
   xticks = c(4,14,23);
   xtickslab=c("4","14","23");
   xylimy = c(-4,-0.5);
   yticks = c(-4,-2.5,-0.5);
   ytickslab=c("-4","-2.5","-0.5");
   print(c(xylimx,xylimy));
   #browser();
   defaultpar=par();
   mypar=list("cex.axis"=0.5,"cex.lab"=1,"cex.main"=1,"bty"="n","cex"=2,"las"=1,"xaxs"="i","mar"=c(4,4,3,3)+0.1);#"pch"=19
   par(mypar);
   for(i in 1:length(groups))
      {
      if(i==1)
         {
         #plot(1:5,pch=24,col="black",bg="grey",cex=3,lwd=2); triangle=24,circle=21,
         plot(xymean[xymean[,bvar]==groups[i],xvar],axes=0,xymean[xymean[,bvar]==groups[i],yvar],type="o",col=xymarkercols[i],
               xlim=xylimx,ylim=xylimy,main=unique(df[,"BrainRegion"]),lwd=xylnwidth,lty=1,pch=marker[i]
               ,xlab="Action potentials",ylab="sAHP amplitude (mV)");
         }
       else
         {
         points(xymean[xymean[,bvar]==groups[i],xvar],xymean[xymean[,bvar]==groups[i],yvar],type="o",col=xymarkercols[i],lwd=xylnwidth,pch=marker[i])
         }
      }
   legend("topright",legend=groups,pch=marker,col=xymarkercols,cex=0.5);
   # plot error bar
   for(i in 1:length(groups))
      {
      xer_x0 = xymean[xymean[,bvar]==groups[i],xvar]-xysem[(xysem[,bvar]==groups[i]),xvar];
      xer_y0 = xymean[xymean[,bvar]==groups[i],yvar];
      xer_x1 = xymean[xymean[,bvar]==groups[i],xvar]+xysem[xysem[,bvar]==groups[i],xvar];
      xer_y1 = xymean[xymean[,bvar]==groups[i],yvar];
      # ----- yerror
      yer_y0 = xymean[xymean[,bvar]==groups[i],yvar]-xysem[(xysem[,bvar]==groups[i]),yvar];
      yer_x0 = xymean[xymean[,bvar]==groups[i],xvar];
      yer_y1 = xymean[xymean[,bvar]==groups[i],yvar]+xysem[xysem[,bvar]==groups[i],yvar];
      yer_x1 = xymean[xymean[,bvar]==groups[i],xvar];
      #print(c(x0,y0,x1,y1));
      segments(xer_x0,xer_y0,xer_x1,xer_y1,col=xysecols[i],lwd=xylnwidth,lty=1);
      segments(yer_x0,yer_y0,yer_x1,yer_y1,col=xysecols[i],lwd=xylnwidth,lty=1);
      }
      axis(1,pos=-4,lwd=2.5,lwd.ticks=2.5,col="black",at=xticks,labels=xtickslab,padj=-0.9,cex.axis=1);
      axis(2,pos=4,lwd=2.5,lwd.ticks=2.5,col="black",at=yticks,labels=ytickslab,cex.axis=1,hadj=0.8);
      par(defaultpar);
   return(xymean);
   };
#ahpo=AHP[AHP$BrainRegion=="PFC-IL" & AHP$Select==1 & AHP$Count==1 & AHP$IClamp >=125 & AHP$IClamp <= 450,];
#ahpo=AHP[AHP$BrainRegion=="OFC-LO" & AHP$SelectOther==1 & AHP$SelectIR==1 & AHP$SelectSR==1 & AHP$SelectAp==1 & AHP$Count==1 & AHP$IClamp >=125 & AHP$IClamp <= 450,];
#ahpo1=dfAmeliaMI(AHP[AHP$BrainRegion=="OFC-LO" & AHP$Count==1 & AHP$Select==1 & AHP$IClamp >=125 & AHP$IClamp <= 450,],"FileID","PeakAhpSf","IClamp","GroupName",500,"yes",sdtimes=2.7)

#jpeg("~/DATA/AHP/AHP_Final/AHPReviewPlots/PFC-IL_ApTotal-PeakAhpSf.jpg",width=12,height=12,units="cm",bg="white",res=300)
#test = xyerror(ahpo,"ApTotal","PeakAhpSa","IClamp","GroupName","CellID")
#dev.off();
