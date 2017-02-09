#---------------------Summary data for ploting
statNAratioAmp<-ddply(els, c("GroupName.pre70"), function(df) return(c(avg=mean(df$AmpRatio), sem=se(df$AmpRatio))))
statNAratioArea<-ddply(els, c("GroupName.pre70"), function(df) return(c(avg=mean(df$AreaRatio), sem=se(df$AreaRatio))))
statPeakpre70<-ddply(els, c("GroupName.pre70"), function(df) return(c(avg=mean(df$Peak.pre70), sem=se(df$Peak.pre70))))
statPeakpre40<-ddply(els, c("GroupName.pre40"), function(df) return(c(avg=mean(df$Peak.pre40), sem=se(df$Peak.pre40))))
statTailpre70<-ddply(els, c("GroupName.pre70"), function(df) return(c(avg=mean(df$Tail.pre70), sem=se(df$Tail.pre70))))
statTailpre40<-ddply(els, c("GroupName.pre40"), function(df) return(c(avg=mean(df$Tail.pre40), sem=se(df$Tail.pre40))))
statPostPre<-ddply(els, c("GroupName.post70"), function(df) return(c(avg=mean(df$PostPre), sem=se(df$PostPre))))
# -------------------- Order Factors
# Order Factors for NAratioAmp
statNAratioAmp$GroupName.pre70 = as.factor(statNAratioAmp$GroupName.pre70)
statNAratioAmp$GroupName.pre70 = reorder(statNAratioAmp$GroupName.pre70, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Order Factors for NAratioArea
statNAratioArea$GroupName.pre70 = as.factor(statNAratioArea$GroupName.pre70)
statNAratioArea$GroupName.pre70 = reorder(statNAratioArea$GroupName.pre70, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Order Factors for Peak70
statPeakpre70$GroupName.pre70 = as.factor(statPeakpre70$GroupName.pre70)
statPeakpre70$GroupName.pre70 = reorder(statPeakpre70$GroupName.pre70, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Order Factors for Peak40
statPeakpre40$GroupName.pre40 = as.factor(statPeakpre40$GroupName.pre40)
statPeakpre40$GroupName.pre40 = reorder(statPeakpre40$GroupName.pre40, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Order Factors for Tail70
statTailpre70$GroupName.pre70 = as.factor(statTailpre70$GroupName.pre70)
statTailpre70$GroupName.pre70 = reorder(statTailpre70$GroupName.pre70, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Order Factors for Tail40
statTailpre40$GroupName.pre40 = as.factor(statTailpre40$GroupName.pre40)
statTailpre40$GroupName.pre40 = reorder(statTailpre40$GroupName.pre40, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Order Factors for PostPre
statPostPre$GroupName.post70 = as.factor(statPostPre$GroupName.post70)
statPostPre$GroupName.post70 = reorder(statPostPre$GroupName.post70, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
#--------------------Data set for ploting postpre with data point connected by lines


#---------------------Ploting pre70peak : for JNS
#create the barplot component AMPA peak
dodge2 <- position_dodge(width=-0.5,height=-0.5)
#dodge2 <- position_dodge("dodge")
plot.pre70peak<-qplot(GroupName.pre70, avg, fill=factor(GroupName.pre70), data=statPeakpre70, geom="bar",color="black",position=dodge2)+
#		scale_y_continuous(name="")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		scale_y_continuous(name="")+scale_x_discrete(name="")+guides(fill=FALSE)+



		theme(axis.text.y = element_text(size=25, hjust=0.5,angle=0), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=15, angle=0,hjust= 0.5))+
#		scale_fill_grey(start = 1, end = 0)+
		scale_fill_manual("Groups", breaks = levels(factor(statPeakpre70$GroupName.pre70)), values= c("white","#D2D2D2","#6E6E6E","#282828") )+
		scale_colour_manual("Groups", breaks = levels(factor(statPeakpre70$GroupName.pre70)), values= c("black","black","black","black") )
#		annotate(geom="text",x=0.5,y=-360,label = "(p > 0.3, Two way ANOVA)",size=5,fontface="plain",color="black",hjust=0)
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.pre70peak= plot.pre70peak+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
#plot.pre70 = plot.pre70+layer(data=els,mapping=aes(x=GroupName.pre70,y=Peak.pre70),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+
plot.pre70peak = plot.pre70peak+layer(data=els,mapping=aes(x=GroupName.pre70,y=Peak.pre70),geom="point", width=1,size=3,color="black",position=position_jitter(w = 0.05, h = 0.05))
#		theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
plot.pre70peak = plot.pre70peak+#theme_bw()+
theme(
    plot.background = element_blank()
   ,panel.grid.major = element_blank()
   ,panel.grid.minor = element_blank()
   ,panel.border = element_blank()
   ,panel.background = element_blank(),
	axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none"
  ) +

  #draws x and y axis line
theme(axis.line = element_blank())+
coord_cartesian(ylim=c(0, -400))+
geom_vline(xintercept=0, colour="black", size=1.5)+
		theme(axis.title.x = element_blank())
#  theme(axis.line = element_line(color = 'black'))
#plot.pre70=plot.pre70+theme(panel.background = element_rect(fill='white', colour='black'))
#tiff("~/DATA/ELS/ELSpre70peak",width=10,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)
#=================================================================================================================================================================
#---------------------Ploting pre40Tail
#create the barplot component AMPA peak
dodge2 <- position_dodge(width=-0.5,height=-0.5)
#dodge2 <- position_dodge("dodge")
plot.pre40tail<-qplot(GroupName.pre40, avg, fill=factor(GroupName.pre40), data=statTailpre40, geom="bar",color="black",position=dodge2)+
#		scale_y_continuous(name="")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		scale_y_continuous(name="")+scale_x_discrete(name="")+guides(fill=FALSE)+



		theme(axis.text.y = element_text(size=25, hjust=0.5,angle=0), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=15, angle=0,hjust= 0.5))+
#		scale_fill_grey(start = 1, end = 0)+
		scale_fill_manual("Groups", breaks = levels(factor(statTailpre40$GroupName.pre40)), values= c("white","#D2D2D2","#6E6E6E","#282828") )+
		scale_colour_manual("Groups", breaks = levels(factor(statTailpre40$GroupName.pre40)), values= c("black","black","black","black") )
#		annotate(geom="text",x=0.5,y=-360,label = "(p > 0.3, Two way ANOVA)",size=5,fontface="plain",color="black",hjust=0)
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.pre40tail= plot.pre40tail+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
#plot.pre70 = plot.pre70+layer(data=els,mapping=aes(x=GroupName.pre70,y=Peak.pre70),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+
plot.pre40tail = plot.pre40tail+layer(data=els,mapping=aes(x=GroupName.pre40,y=Tail.pre40),geom="point", width=1,size=3,color="black",position=position_jitter(w = 0.05, h = 0.05))
#		theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
plot.pre40tail = plot.pre40tail+#theme_bw()+
theme(
    plot.background = element_blank()
   ,panel.grid.major = element_blank()
   ,panel.grid.minor = element_blank()
   ,panel.border = element_blank()
   ,panel.background = element_blank(),
	axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none"
  ) +

  #draws x and y axis line
theme(axis.line = element_blank())+
coord_cartesian(ylim=c(0, 200))+
geom_vline(xintercept=0, colour="black", size=1.5)+
		theme(axis.title.x = element_blank())
#  theme(axis.line = element_line(color = 'black'))
#plot.pre70=plot.pre70+theme(panel.background = element_rect(fill='white', colour='black'))
#tiff("~/DATA/ELS/ELSpre40Tail",width=10,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)
#=================================================================================================================================================================
#---------------------Ploting postpre
#create the barplot component AMPA peak
dodge2 <- position_dodge(width=-0.5,height=-0.5)
#dodge2 <- position_dodge("dodge")
plot.postpre<-qplot(GroupName.post70, avg, fill=factor(GroupName.post70), data=statPostPre, geom="bar",color="black",position=dodge2)+
#		scale_y_continuous(name="")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		scale_y_continuous(name="")+scale_x_discrete(name="")+guides(fill=FALSE)+



		theme(axis.text.y = element_text(size=25, hjust=0.5,angle=0), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=15, angle=0,hjust= 0.5))+
#		scale_fill_grey(start = 1, end = 0)+
		scale_fill_manual("Groups", breaks = levels(factor(statPostPre$GroupName.post70)), values= c("white","#D2D2D2","#6E6E6E","#282828") )+
		scale_colour_manual("Groups", breaks = levels(factor(statPostPre$GroupName.post70)), values= c("black","black","black","black") )
#		annotate(geom="text",x=0.5,y=-360,label = "(p > 0.3, Two way ANOVA)",size=5,fontface="plain",color="black",hjust=0)
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.postpre= plot.postpre+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
#plot.pre70 = plot.pre70+layer(data=els,mapping=aes(x=GroupName.pre70,y=Peak.pre70),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+
plot.postpre = plot.postpre+layer(data=els,mapping=aes(x=GroupName.post70,y=PostPre),geom="point", width=1,size=3,color="black",position=position_jitter(w = 0.05, h = 0.05))
#		theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
plot.postpre = plot.postpre+#theme_bw()+
theme(
    plot.background = element_blank()
   ,panel.grid.major = element_blank()
   ,panel.grid.minor = element_blank()
   ,panel.border = element_blank()
   ,panel.background = element_blank(),
	axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none"
  ) +

  #draws x and y axis line
theme(axis.line = element_blank())+
coord_cartesian(ylim=c(0, 300))+
geom_vline(xintercept=0, colour="black", size=1.5)+
		theme(axis.title.x = element_blank())
#  theme(axis.line = element_line(color = 'black'))
#plot.pre70=plot.pre70+theme(panel.background = element_rect(fill='white', colour='black'))
#tiff("~/DATA/ELS/ELSpostpre",width=10,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)
 #==============================================================================================================================================================
 #--------------------Line Plot AMPA Pre & AMPA Post
# don't use cbind as it converts all variables into a single datatype such as string
ltpwide = data.frame("CellID"=els$CellID,"GroupName"=els$GroupName.post70,"Peak1"=els$Peak.pre70,"Peak2"=els$Peak.post70);
# remove colums with NA in Peak2
ltpwide = ltpwide[!is.na(ltpwide[,"Peak2"]),];
ltplong=reshape(ltpwide, direction = "long", varying =c("Peak1","Peak2"), timevar="Cond", sep="")
# remove additional columns created by reshape: row.names, id
ltplong = ltplong[,c("CellID","GroupName","Cond","Peak")];
# Create additional factor variables: in_vivo and ex_vivo
factorsltp=str_split_fixed(ltplong$GroupName,"\\+",2)
ltplong = cbind(ltplong,"Treat_invivo"=as.character(factorsltp[,1]),"Treat_exvivo"=as.character(factorsltp[,2]))
# include only one group
ltpels = ltplong[grepl(pattern="ELS",ltplong[,"GroupName"]),];
ltpctrl = ltplong[grepl(pattern="CTRL",ltplong[,"GroupName"]),];
ltpctrl[,"CellID"] = as.character(ltpctrl[,"CellID"])
 #-------------------------------------------------------- ploting ltpctrl
plot.ctrl = ggplot(data=ltpctrl,aes(y=Peak, x=Cond,group=CellID))+
		geom_line() + # join points with lines (specify this before geom_point, or the lines will be drawn over the shapes)
		geom_point(aes(shape=Treat_exvivo, fill=Treat_exvivo),size=5) +  # add a scatterplot; constant size, shape/fill depends on lesion
		scale_x_continuous("", breaks=c(1,2),limits=c(0.8,3.5)) + # have tick marks for each Cond
		scale_y_continuous("", limits = c(-400, 0), breaks=seq(-400,0, by = 50)) + # rescale Y axis slightly
		scale_shape_manual(values=c(21,21)) + # explicitly have sham=fillable triangle, ACCX=fillable circle
     		scale_fill_manual(values=c("white","black")) + # explicitly have sham=white, ACCX=black
		theme(axis.text.y = element_text(size=25, hjust=0.5,angle=0,color="black"), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
				axis.line = element_line(color="black",size=1),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain",color="black"), axis.title.x = element_text(size=15, angle=0,hjust= 0.5))
		#theme_bw()  # make the theme black-and-white rather than grey (do this before font changes, or it overrides them)
plot.ctrl = plot.ctrl+theme(
    		 plot.background = element_blank()
   		,panel.grid.major = element_blank()
   		,panel.grid.minor = element_blank()
   		,panel.border = element_blank()
   		,panel.background = element_blank(),
		axis.text.x=element_blank(),
        	axis.ticks.x=element_blank(),legend.position="none"
        #	axis.title.x=element_blank(),
        #	axis.title.y=element_blank(),legend.position="none"
  )

  #draws x and y axis line
#theme(axis.line = element_blank())+
#coord_cartesian(ylim=c(0, -350))+
#geom_vline(xintercept=0.8, colour="black", size=1.5)+
#		theme(axis.title.x = element_blank())

#-------------------------------------------------------- ploting ltpels
plot.els = ggplot(data=ltpels,aes(y=Peak, x=Cond,group=CellID))+
		geom_line() + # join points with lines (specify this before geom_point, or the lines will be drawn over the shapes)
		geom_point(aes(shape=Treat_exvivo, fill=Treat_exvivo),size=5) +  # add a scatterplot; constant size, shape/fill depends on lesion
		scale_x_continuous("", breaks=c(1,2),limits=c(0.8,3.5)) + # have tick marks for each Cond
		scale_y_continuous("", limits = c(-400, 0), breaks=seq(-400,0, by = 50)) + # rescale Y axis slightly
		scale_shape_manual(values=c(21,21)) + # explicitly have sham=fillable triangle, ACCX=fillable circle
     		scale_fill_manual(values=c("white","black")) + # explicitly have sham=white, ACCX=black
		theme(axis.text.y = element_text(size=25, hjust=0.5,angle=0,color="black"), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
				axis.line = element_line(color="black",size=1),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain",color="black"), axis.title.x = element_text(size=15, angle=0,hjust= 0.5))
		#theme_bw()  # make the theme black-and-white rather than grey (do this before font changes, or it overrides them)
plot.els = plot.els+theme(
    		 plot.background = element_blank()
   		,panel.grid.major = element_blank()
   		,panel.grid.minor = element_blank()
   		,panel.border = element_blank()
   		,panel.background = element_blank(),
		axis.text.x=element_blank(),
        	axis.ticks.x=element_blank(),legend.position="none"
        #	axis.title.x=element_blank(),
        #	axis.title.y=element_blank(),legend.position="none"
  )
#tiff("~/DATA/ELS/ELS_ltpctrl",width=10,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)
#=================================================================================================================================================================
#---------------------Ploting AmpRatio
#create the barplot component AmpRatio
dodge2 <- position_dodge(width=-0.5,height=-0.5)
#dodge2 <- position_dodge("dodge")
plot.naratioamp<-qplot(GroupName.pre70, avg, fill=factor(GroupName.pre70), data=statNAratioAmp, geom="bar",color="black",position=dodge2)+
#		scale_y_continuous(name="")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		scale_y_continuous(name="")+scale_x_discrete(name="")+guides(fill=FALSE)+



		theme(axis.text.y = element_text(size=25, hjust=0.5,angle=0), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=15, angle=0,hjust= 0.5))+
#		scale_fill_grey(start = 1, end = 0)+
		scale_fill_manual("Groups", breaks = levels(factor(statNAratioAmp$GroupName.pre70)), values= c("white","#D2D2D2","#6E6E6E","#282828") )+
		scale_colour_manual("Groups", breaks = levels(factor(statNAratioAmp$GroupName.pre70)), values= c("black","black","black","black") )
#		annotate(geom="text",x=0.5,y=-360,label = "(p > 0.3, Two way ANOVA)",size=5,fontface="plain",color="black",hjust=0)
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.naratioamp = plot.naratioamp+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
#plot.pre70 = plot.pre70+layer(data=els,mapping=aes(x=GroupName.pre70,y=Peak.pre70),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+
plot.naratioamp = plot.naratioamp+layer(data=els,mapping=aes(x=GroupName.pre70,y=AmpRatio),geom="point", width=1,size=3,color="black",position=position_jitter(w = 0.05, h = 0.05))
#		theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
plot.naratioamp = plot.naratioamp+#theme_bw()+
theme(
    plot.background = element_blank()
   ,panel.grid.major = element_blank()
   ,panel.grid.minor = element_blank()
   ,panel.border = element_blank()
   ,panel.background = element_blank(),
	axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none"
  ) +

  #draws x and y axis line
theme(axis.line = element_blank())+
coord_cartesian(ylim=c(0, 1))+
geom_vline(xintercept=0, colour="black", size=1.5)+
		theme(axis.title.x = element_blank())
#  theme(axis.line = element_line(color = 'black'))
#plot.pre70=plot.pre70+theme(panel.background = element_rect(fill='white', colour='black'))
#tiff("~/DATA/ELS/ELSnaratioamp",width=10,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)
