if (exists("els") == 0) stop ('The dataframe "els" is missing!');
#---------------------Summary data for ploting
stat.All<-ddply(els, c("GroupName.pre70"), function(df) return(c(avg.AmpRatio=mean(df$AmpRatio), sem.AmpRatio=se(df$AmpRatio),
									    					 avg.PeakAMPA=mean(df$Peak.pre70), sem.PeakAMPA=se(df$Peak.pre70),
														 avg.TailNMDA=mean(df$Tail.pre40), sem.TailNMDA=se(df$Tail.pre40),
														 avg.InputRes=mean(df$InputRes.pre70), sem.InputRes=se(df$InputRes.pre70),
														 avg.SeriesRes=mean(df$SeriesRes.pre70), sem.SeriesRes=se(df$SeriesRes.pre70),
														 avg.TauMem=mean(df$TauMem.pre70), sem.SeriesRes=se(df$mTau.pre70),
														 avg.Age=mean(df$Age), sem.Age=se(df$Age),
														 avg.ExpDelay=mean(df$ExpDelay), sem.ExpDelay=se(df$ExpDelay)
)))

	#---------------------Summary data for ploting
stat.NAratioAmp<-ddply(els, c("GroupName.pre70"), function(df) return(c(avg=mean(df$AmpRatio), sem=se(df$AmpRatio))))
stat.NAratioArea<-ddply(els, c("GroupName.pre70"), function(df) return(c(avg=mean(df$AreaRatio), sem=se(df$AreaRatio))))
stat.Peakpre70<-ddply(els, c("GroupName.pre70"), function(df) return(c(avg=mean(df$Peak.pre70), sem=se(df$Peak.pre70))))
stat.Peakpre40<-ddply(els, c("GroupName.pre40"), function(df) return(c(avg=mean(df$Peak.pre40), sem=se(df$Peak.pre40))))
stat.Tailpre70<-ddply(els, c("GroupName.pre70"), function(df) return(c(avg=mean(df$Tail.pre70), sem=se(df$Tail.pre70))))
stat.Tailpre40<-ddply(els, c("GroupName.pre40"), function(df) return(c(avg=mean(df$Tail.pre40), sem=se(df$Tail.pre40))))
	#---------------------Ploting NMDA /AMPA Ratio

# Order Factors for Peak70
stat.Peakpre70$GroupName.pre70 = as.factor(stat.Peakpre70$GroupName.pre70)
stat.Peakpre70$GroupName.pre70 = reorder(stat.Peakpre70$GroupName.pre70, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Order Factors for Tail70
stat.Tailpre70$GroupName.pre70 = as.factor(stat.Tailpre70$GroupName.pre70)
stat.Tailpre70$GroupName.pre70 = reorder(stat.Tailpre70$GroupName.pre70, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Order Factors for Peak40
stat.Peakpre40$GroupName.pre40 = as.factor(stat.Peakpre40$GroupName.pre40)
stat.Peakpre40$GroupName.pre40 = reorder(stat.Peakpre40$GroupName.pre40, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Order Factors for Tail40
stat.Tailpre40$GroupName.pre40 = as.factor(stat.Tailpre40$GroupName.pre40)
stat.Tailpre40$GroupName.pre40 = reorder(stat.Tailpre40$GroupName.pre40, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Order Factors for NAratioAmp
stat.NAratioAmp$GroupName.pre70 = as.factor(stat.NAratioAmp$GroupName.pre70)
stat.NAratioAmp$GroupName.pre70 = reorder(stat.NAratioAmp$GroupName.pre70, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Order Factors for NAratioArea
stat.NAratioArea$GroupName.pre70 = as.factor(stat.NAratioArea$GroupName.pre70)
stat.NAratioArea$GroupName.pre70 = reorder(stat.NAratioArea$GroupName.pre70, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))



#create the barplot component AMPA peak
plot.pre70<-qplot(GroupName.pre70, avg, fill=factor(GroupName.pre70), data=stat.Peakpre70, geom="bar", position="dodge")+
		scale_y_continuous(name="AMPA Epsc Peak (pA) ")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		theme(axis.text.y = element_text(size=16, hjust=0.5,angle=0), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=15, angle=0,hjust= 0.5))+
		scale_fill_manual("Groups", breaks = levels(factor(stat.Peakpre70$GroupName.pre70)), values= c("lightgrey","black","pink","red") )+
		scale_colour_manual("Groups", breaks = levels(factor(stat.Peakpre70$GroupName.pre70)), values= c("black","black","black","black") )+
		annotate(geom="text",x=0.5,y=-360,label = "(p > 0.3, Two way ANOVA)",size=5,fontface="plain",color="black",hjust=0)
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.pre70= plot.pre70+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
plot.pre70 = plot.pre70+layer(data=els,mapping=aes(x=GroupName.pre70,y=Peak.pre70),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+
		theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
plot.pre70=plot.pre70+theme(panel.background = element_rect(fill='white', colour='black'))
#tiff("~/DATA/ELS/ELS_Peak_AMPA_20121211",width=17,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)


	#-----------------End NMDA/AMPA Ratio plot

#create the barplot component NMDA peak
plot.pre40<-qplot(GroupName.pre40, avg, fill=factor(GroupName.pre40), data=stat.Peakpre40, geom="bar", position="dodge")+
		scale_y_continuous(name="NMDA Peak Epsc Peak (pA) ")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		theme(axis.text.y = element_text(size=16, hjust=0.5,angle=0), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=16, angle=0,hjust= 0.5))+
		scale_fill_manual("Groups", breaks = levels(factor(stat.Peakpre40$GroupName.pre40)), values= c("purple","blue","magenta","red") )+
		annotate(geom="text",x=0.5,y=465,label = "Main effect of ELS (p = 0.13)",size=5,fontface="plain",color="black",hjust=0)+
		annotate(geom="text",x=0.5,y=430,label = "F(1,30) = 2.4, Two way ANOVA",size=5,fontface="plain",color="black",hjust=0)
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.pre40= plot.pre40+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
plot.pre40 = plot.pre40+layer(data=els,mapping=aes(x=GroupName.pre40,y=Peak.pre40),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+
		theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
plot.pre40=plot.pre40+theme(panel.background = element_rect(fill='white', colour='black'))
#tiff("~/DATA/ELS/ELS_NMDA_2012103",width=17,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)

#======================create the simple barplot component NARatio Amp
plot.NAratioAmp<-qplot(GroupName.pre70, avg, fill=factor(GroupName.pre70), data=stat.NAratioAmp, geom="bar", position="dodge")+
		scale_y_continuous(name="Amplitude Ratio (NMDA Epsc Tail /AMPA Epsc Peak) ")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		theme(axis.text.y = element_text(size=16, hjust=0.5,angle=0), axis.title.y = element_text(size=15, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=16, angle=0,hjust= 0.5))+
		scale_fill_manual("Groups", breaks = levels(factor(stat.NAratioAmp$GroupName.pre70)), values= c("lightgrey","black","pink","red") )+
		scale_colour_manual("Groups", breaks = levels(factor(stat.NAratioAmp$GroupName.pre70)), values= c("black","black","black","black") )+
		annotate(geom="text",x=0.5,y=1.2,label = "Main effect of ELS (p = 0.034)",size=5,fontface="plain",color="black",hjust=0)+
		annotate(geom="text",x=0.5,y=1.13,label = "F(1,40) = 4.82, Two way ANOVA",size=5,fontface="plain",color="black",hjust=0)
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.NAratioAmp= plot.NAratioAmp+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), data=stat.NAratioAmp,position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
plot.NAratioAmp = plot.NAratioAmp+layer(data=els,mapping=aes(x=GroupName.pre70,y=AmpRatio),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+
		theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
plot.NAratioAmp=plot.NAratioAmp+theme(panel.background = element_rect(fill='white', colour='black'))
#tiff("~/DATA/ELS/ELS_NAratioAmp_20121211",width=17,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)

#======================create the simple barplot component NARatio Area
plot.NAratioArea<-qplot(GroupName.pre70, avg, fill=factor(GroupName.pre70), data=stat.NAratioArea, geom="bar", position="dodge")+
		scale_y_continuous(name="Area Ratio (NMDA Epsc Tail /AMPA Epsc Peak) ")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		theme(axis.text.y = element_text(size=16, hjust=0.5,angle=0), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=16, angle=0,hjust= 0.5))+
		scale_fill_manual("Groups", breaks = levels(factor(stat.NAratioArea$GroupName.pre70)), values= c("lightgrey","black","pink","red") )+
		scale_colour_manual("Groups", breaks = levels(factor(stat.NAratioArea$GroupName.pre70)), values= c("black","black","black","black") )+
		annotate(geom="text",x=0.5,y=4.0,label = "Main effect of ELS (p = 0.094)",size=5,fontface="plain",color="black",hjust=0)+
		annotate(geom="text",x=0.5,y=3.8,label = "F(1,40) = 2.943, Two way ANOVA",size=5,fontface="plain",color="black",hjust=0)
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.NAratioArea= plot.NAratioArea+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), data=stat.NAratioArea,position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
plot.NAratioArea = plot.NAratioArea+layer(data=els,mapping=aes(x=GroupName.pre70,y=AreaRatio),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+
		theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
plot.NAratioArea=plot.NAratioArea+theme(panel.background = element_rect(fill='white', colour='black'))
#tiff("~/DATA/ELS/ELS_NAratioArea_20121211",width=17,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)

#==========create the barplot component NMDA Tail
plot.pre40tail<-qplot(GroupName.pre40, avg, fill=factor(GroupName.pre40), data=stat.Tailpre40, geom="bar", position="dodge")+
		scale_y_continuous(name="NMDA Tail Epsc Peak (pA) ")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		theme(axis.text.y = element_text(size=16, hjust=0.5,angle=0), axis.title.y = element_text(size=15, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=15, angle=0,hjust= 0.5))+
		scale_fill_manual("Groups", breaks = levels(factor(stat.Tailpre40$GroupName.pre40)), values= c("lightgrey","black","pink","red") )+
		scale_colour_manual("Groups", breaks = levels(factor(stat.Tailpre40$GroupName.pre40)), values= c("black","black","black","black") )+
		annotate(geom="text",x=0.5,y=260,label = "Main effect of ELS (p = 0.039*)",size=5,fontface="plain",color="black",hjust=0)+
		annotate(geom="text",x=0.5,y=247,label = "F(1,40) = 4.543, Two way ANOVA",size=5,fontface="plain",color="black",hjust=0)
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.pre40tail= plot.pre40tail+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
plot.pre40tail = plot.pre40tail+layer(data=els,mapping=aes(x=GroupName.pre40,y=Tail.pre40),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+
theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
#tiff("~/DATA/ELS/ELS_Tail_NMDA_20121211",width=17,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)

  #--------------------Plot AMPA Post/ AMPA Pre

stat.PostPre<-ddply(els, c("GroupName.post70"), function(df) return(c(avg=mean(df$PostPre), sem=se(df$PostPre))))
elspostpre = els[!is.na(els[,"GroupName.post70"]),]
# Order Factors
stat.PostPre$GroupName.post70 = as.factor(stat.PostPre$GroupName.post70)
stat.PostPre$GroupName.post70 = reorder(stat.PostPre$GroupName.post70, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))

elspostpre$GroupName.post70 = as.factor(elspostpre$GroupName.post70)
elspostpre$GroupName.post70 = reorder(elspostpre$GroupName.post70, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))


stat.PostPre = stat.PostPre[!is.na(stat.PostPre[,"GroupName.post70"]),]

plot.postpre<-qplot(GroupName.post70, avg, fill=factor(GroupName.post70), data=stat.PostPre, geom="bar", position="dodge")+
		geom_hline(yintercept=1, colour="black", lwd=0.3,linetype="dashed")+
		scale_y_continuous(name="Post AMPA / Pre AMPA ")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		theme(axis.text.y = element_text(size=16, hjust=0.5,angle=0), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=16, angle=0,hjust= 0.5))+
		scale_fill_manual("Groups", breaks = levels(factor(stat.PostPre$GroupName.post70)), values= c("darkgrey","black","magenta","red") )+
		annotate(geom="text",x=0.5,y=2.8,label = "(p > 0.7, Two way ANOVA)",size=5,fontface="plain",color="black",hjust=0)
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.postpre= plot.postpre+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
plot.postpre = plot.postpre+layer(data=elspostpre,mapping=aes(x=GroupName.post70,y=PostPre),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+
theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
#tiff("~/DATA/ELS/ELS_PostPre",width=17,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)

 #--------------------Line Plot AMPA Pre & AMPA Post

df.elsltpwide = cbind(els$CellID,els$GroupName.post70,els$Peak.pre70,els$Peak.post70);

stat.PostPre<-ddply(els, c("GroupName.post70"), function(df) return(c(avg=mean(df$PostPre), sem=se(df$PostPre))))
elspostpre = els[!is.na(els[,"GroupName.post70"]),]
# Order Factors
stat.PostPre$GroupName.post70 = as.factor(stat.PostPre$GroupName.post70)
stat.PostPre$GroupName.post70 = reorder(stat.PostPre$GroupName.post70, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))

elspostpre$GroupName.post70 = as.factor(elspostpre$GroupName.post70)
elspostpre$GroupName.post70 = reorder(elspostpre$GroupName.post70, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))


stat.PostPre = stat.PostPre[!is.na(stat.PostPre[,"GroupName.post70"]),]

plot.postpre<-qplot(GroupName.post70, avg, fill=factor(GroupName.post70), data=stat.PostPre, geom="bar", position="dodge")+
		geom_hline(yintercept=1, colour="black", lwd=0.3,linetype="dashed")+
		scale_y_continuous(name="Post AMPA / Pre AMPA ")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		theme(axis.text.y = element_text(size=16, hjust=0.5,angle=0), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=16, angle=0,hjust= 0.5))+
		scale_fill_manual("Groups", breaks = levels(factor(stat.PostPre$GroupName.post70)), values= c("darkgrey","black","magenta","red") )+
		annotate(geom="text",x=0.5,y=2.8,label = "(p > 0.7, Two way ANOVA)",size=5,fontface="plain",color="black",hjust=0)
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.postpre= plot.postpre+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
plot.postpre = plot.postpre+layer(data=elspostpre,mapping=aes(x=GroupName.post70,y=PostPre),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+
theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
#tiff("~/DATA/ELS/ELS_PostPre",width=17,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)

