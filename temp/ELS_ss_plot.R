#---------------------Summary data for ploting
statnss70100PPF<-ddply(nss70100pamp, c("GroupName"), function(df) return(c(avg=mean(df$PPF), sem=se(df$PPF))))
statnss70100TS10<-ddply(nss70100pamp, c("GroupName"), function(df) return(c(avg=mean(df$TS10), sem=se(df$TS10))))
statnss40100PPF<-ddply(nss40100pamp, c("GroupName"), function(df) return(c(avg=mean(df$PPF), sem=se(df$PPF))))
statnss40100TS10<-ddply(nss40100pamp, c("GroupName"), function(df) return(c(avg=mean(df$TS10), sem=se(df$TS10))))

# -------------------- Order Factors 
# Order Factors for statnss70100PPF
statnss70100PPF$GroupName = as.factor(statnss70100PPF$GroupName)
statnss70100PPF$GroupName = reorder(statnss70100PPF$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Order Factors for statnss70100TS10
statnss70100TS10$GroupName = as.factor(statnss70100TS10$GroupName)
statnss70100TS10$GroupName = reorder(statnss70100TS10$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Order Factors for statnss40100PPF
statnss40100PPF$GroupName = as.factor(statnss40100PPF$GroupName)
statnss40100PPF$GroupName = reorder(statnss40100PPF$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Order Factors for statnss40100TS10
statnss40100TS10$GroupName = as.factor(statnss40100TS10$GroupName)
statnss40100TS10$GroupName = reorder(statnss40100TS10$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))	

#---------------------Ploting nss70100PPF : for JNS

dodge2 <- position_dodge(width=-0.5,height=-0.5) 
#dodge2 <- position_dodge("dodge") 
plot.nss70100PPF<-qplot(GroupName, avg, fill=factor(GroupName), data=statnss70100PPF, geom="bar",color="black",position=dodge2)+
#		scale_y_continuous(name="")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		scale_y_continuous(name="")+scale_x_discrete(name="")+guides(fill=FALSE)+



		theme(axis.text.y = element_text(size=25, hjust=0.5,angle=0,color="black"), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain",color="black"), axis.title.x = element_text(size=15, angle=0,hjust= 0.5))+
#		scale_fill_grey(start = 1, end = 0)+
		scale_fill_manual("Groups", breaks = levels(factor(statnss70100PPF$GroupName)), values= c("white","#D2D2D2","#6E6E6E","#282828") )+
		scale_colour_manual("Groups", breaks = levels(factor(statnss70100PPF$GroupName)), values= c("black","black","black","black") )
#		annotate(geom="text",x=0.5,y=-360,label = "(p > 0.3, Two way ANOVA)",size=5,fontface="plain",color="black",hjust=0)	
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)  
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.nss70100PPF= plot.nss70100PPF+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
#plot.pre70 = plot.pre70+layer(data=els,mapping=aes(x=GroupName.pre70,y=Peak.pre70),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+ 
plot.nss70100PPF = plot.nss70100PPF+layer(data=subset(nss70100pamp,StimNum==1),mapping=aes(x=GroupName,y=PPF),geom="point", width=1,size=3,color="black",position=position_jitter(w = 0.05, h = 0.05))
#		theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
plot.nss70100PPF = plot.nss70100PPF+#theme_bw()+
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
coord_cartesian(ylim=c(0, 400))+  
geom_vline(xintercept=0, colour="black", size=1.5)+
		theme(axis.title.x = element_blank())
#  theme(axis.line = element_line(color = 'black'))
#tiff("~/DATA/ELS/ELSss70100PPF",width=10,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)

# ====================================================================================================================================================================

#---------------------Ploting nss70100TS10 : for JNS

dodge2 <- position_dodge(width=-0.5,height=-0.5) 
#dodge2 <- position_dodge("dodge") 
plot.nss70100TS10<-qplot(GroupName, avg, fill=factor(GroupName), data=statnss70100TS10, geom="bar",color="black",position=dodge2)+
#		scale_y_continuous(name="")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		scale_y_continuous(name="")+scale_x_discrete(name="")+guides(fill=FALSE)+



		theme(axis.text.y = element_text(size=25, hjust=0.5,angle=0,color="black"), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=15, angle=0,hjust= 0.5))+
#		scale_fill_grey(start = 1, end = 0)+
		scale_fill_manual("Groups", breaks = levels(factor(statnss70100TS10$GroupName)), values= c("white","#D2D2D2","#6E6E6E","#282828") )+
		scale_colour_manual("Groups", breaks = levels(factor(statnss70100TS10$GroupName)), values= c("black","black","black","black") )
#		annotate(geom="text",x=0.5,y=-360,label = "(p > 0.3, Two way ANOVA)",size=5,fontface="plain",color="black",hjust=0)	
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)  
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.nss70100TS10= plot.nss70100TS10+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
#plot.pre70 = plot.pre70+layer(data=els,mapping=aes(x=GroupName.pre70,y=Peak.pre70),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+ 
plot.nss70100TS10 = plot.nss70100TS10+layer(data=subset(nss70100pamp,StimNum==1),mapping=aes(x=GroupName,y=TS10),geom="point", width=1,size=3,color="black",position=position_jitter(w = 0.05, h = 0.05))
#		theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
plot.nss70100TS10 = plot.nss70100TS10+#theme_bw()+
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
coord_cartesian(ylim=c(0, 400))+  
geom_vline(xintercept=0, colour="black", size=1.5)+
		theme(axis.title.x = element_blank())
#  theme(axis.line = element_line(color = 'black'))
#tiff("~/DATA/ELS/ELSss70100TS10",width=10,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)

# ======================================================================================================================================================================

#---------------------Ploting nss40100PPF : for JNS

dodge2 <- position_dodge(width=-0.5,height=-0.5) 
#dodge2 <- position_dodge("dodge") 
plot.nss40100PPF<-qplot(GroupName, avg, fill=factor(GroupName), data=statnss40100PPF, geom="bar",color="black",position=dodge2)+
#		scale_y_continuous(name="")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		scale_y_continuous(name="")+scale_x_discrete(name="")+guides(fill=FALSE)+



		theme(axis.text.y = element_text(size=25, hjust=0.5,angle=0,color="black"), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=15, angle=0,hjust= 0.5))+
#		scale_fill_grey(start = 1, end = 0)+
		scale_fill_manual("Groups", breaks = levels(factor(statnss40100PPF$GroupName)), values= c("white","#D2D2D2","#6E6E6E","#282828") )+
		scale_colour_manual("Groups", breaks = levels(factor(statnss40100PPF$GroupName)), values= c("black","black","black","black") )
#		annotate(geom="text",x=0.5,y=-360,label = "(p > 0.3, Two way ANOVA)",size=5,fontface="plain",color="black",hjust=0)	
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)  
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.nss40100PPF= plot.nss40100PPF+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
#plot.pre70 = plot.pre70+layer(data=els,mapping=aes(x=GroupName.pre70,y=Peak.pre70),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+ 
plot.nss40100PPF = plot.nss40100PPF+layer(data=subset(nss40100pamp,StimNum==1),mapping=aes(x=GroupName,y=PPF),geom="point", width=1,size=3,color="black",position=position_jitter(w = 0.05, h = 0.05))
#		theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
plot.nss40100PPF = plot.nss40100PPF+#theme_bw()+
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
coord_cartesian(ylim=c(0, 400))+  
geom_vline(xintercept=0, colour="black", size=1.5)+
		theme(axis.title.x = element_blank())
#  theme(axis.line = element_line(color = 'black'))
tiff("~/DATA/ELS/ELSss40100PPF",width=10,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)

# ======================================================================================================================================================================

#---------------------Ploting nss40100TS10 : for JNS

dodge2 <- position_dodge(width=-0.5,height=-0.5) 
#dodge2 <- position_dodge("dodge") 
plot.nss40100TS10<-qplot(GroupName, avg, fill=factor(GroupName), data=statnss40100TS10, geom="bar",color="black",position=dodge2)+
#		scale_y_continuous(name="")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		scale_y_continuous(name="")+scale_x_discrete(name="")+guides(fill=FALSE)+



		theme(axis.text.y = element_text(size=25, hjust=0.5,angle=0,color="black"), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=15, angle=0,hjust= 0.5))+
#		scale_fill_grey(start = 1, end = 0)+
		scale_fill_manual("Groups", breaks = levels(factor(statnss40100TS10$GroupName)), values= c("white","#D2D2D2","#6E6E6E","#282828") )+
		scale_colour_manual("Groups", breaks = levels(factor(statnss40100TS10$GroupName)), values= c("black","black","black","black") )
#		annotate(geom="text",x=0.5,y=-360,label = "(p > 0.3, Two way ANOVA)",size=5,fontface="plain",color="black",hjust=0)	
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)  
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.nss40100TS10= plot.nss40100TS10+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
#plot.pre70 = plot.pre70+layer(data=els,mapping=aes(x=GroupName.pre70,y=Peak.pre70),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+ 
plot.nss40100TS10 = plot.nss40100TS10+layer(data=subset(nss40100pamp,StimNum==1),mapping=aes(x=GroupName,y=TS10),geom="point", width=1,size=3,color="black",position=position_jitter(w = 0.05, h = 0.05))
#		theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
plot.nss40100TS10 = plot.nss40100TS10+#theme_bw()+
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
coord_cartesian(ylim=c(-100,200))+  
geom_vline(xintercept=0, colour="black", size=1.5)+
		theme(axis.title.x = element_blank())
#  theme(axis.line = element_line(color = 'black'))
#tiff("~/DATA/ELS/ELSss40100TS10",width=10,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)
