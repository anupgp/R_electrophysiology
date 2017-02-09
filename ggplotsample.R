#load("~/DATA/AHP/R/Rdata/OFC-LO_Power_PeakAhpSf_noBADCells.RData")
#jpeg("~/DATA/AHP/Rebuttal/AHPReviewPlots/OFC-LO_PowerPeakAhpSf.jpg",width=12,height=12,units="cm",bg="white",res=300)
xticks =seq(0,175,50);
xlabels = as.character(xticks);
yticks = seq(0.25,1,0.25);
ylabels = as.character(yticks);
ypos=0
xpos=0.25
powerplot=ggplot(ypowerNnew[ypowerNnew$cells<200,],aes(x=cells,y=PowerInt/100))+geom_smooth(se=FALSE,method="loess",col="grey50",lwd=3)+geom_point(aes(x=35,y=0.38),size=7,pch=22,alpha=1,lwd=5)+
	# explicitly have sham=fillable triangle, ACCX=fillable circle #values=c(24,21)
	theme(axis.text.y = element_text(size=25, hjust=0.5,color="black",angle=0),
	axis.title.y = element_text(size=25, angle=90,vjust=0.45),
	axis.text.x  = element_text(angle=0, hjust=0.5,vjust=0.5, color="black",size=25,face="plain"),
	axis.title.x = element_text(size=25, angle=0,hjust= 0.5))+
	theme(
                                        plot.background = element_blank()
                                        ,panel.grid.major = element_blank()
                                        ,panel.grid.minor = element_blank()
                                        ,panel.border = element_blank()
                                        ,panel.background = element_blank()
                                        #,axis.title.y=element_blank()
					#,axis.title.x=element_blank()
                                        ,legend.position="none"
                                        )+

	scale_y_discrete("", expand=c(0,0),limits=yticks, labels = ylabels) +
	scale_x_discrete("", expand=c(0,0),limits=xticks,labels = xlabels)+
        coord_cartesian(xlim=c(0,175),ylim=yticks)+

	theme(axis.ticks.length = unit(0.25, "cm"),axis.ticks= element_line(size = 1.5,color="black"))+
	geom_vline(aes(xintercept=ypos), colour="black", size=2)+
	geom_hline(aes(yintercept=xpos), colour="black", size=2)+
	#geom_hline(aes(yintercept=0.8), colour="black", size=1,linetype=2)+
	geom_vline(aes(xintercept=70), colour="black", size=1,linetype=2)+
	#geom_vline(aes(xintercept=80), colour="black", size=1,linetype=1)+
	theme(axis.ticks.length = unit(0.25, "cm"),axis.ticks= element_line(size = 1,color="black"))
