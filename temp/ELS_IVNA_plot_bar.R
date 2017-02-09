if (exists("ivna") == 0) stop(' !!!!!!!   Excecution stopped. dataframe: "naiv" missing    !!!!!!'); 
#---------------------Summary data for ploting
statRevPeakCor<-ddply(ivna, c("GroupName.iv"), function(df) return(c(avg=mean(df$RevPeakCor.iv), sem=se(df$RevPeakCor.iv), n = length(df$RevPeakCor))))
statRevTailCor<-ddply(ivna, c("GroupName.iv"), function(df) return(c(avg=mean(df$RevTailCor.iv), sem=se(df$RevTailCor.iv), n = length(df$RevPeakCor))))
# -------------------- Order Factors 
# Order Factors for RevPeakCor
statRevPeakCor$GroupName.iv = as.factor(statRevPeakCor$GroupName.iv);
statRevPeakCor$GroupName.iv = reorder(statRevPeakCor$GroupName.iv, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"));
# Order Factors for RevTailCor
statRevTailCor$GroupName.iv = as.factor(statRevTailCor$GroupName.iv);
statRevTailCor$GroupName.iv = reorder(statRevTailCor$GroupName.iv, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"));
#===================================================================    Ploting RevPeak : for JNS    ==============================================================
dodge2 <- position_dodge(width=-0.5,height=-0.5) 
#dodge2 <- position_dodge("dodge") 
plot.RevPeakCor<-qplot(GroupName.iv, avg, fill=factor(GroupName.iv), data=statRevPeakCor, geom="bar",color="black",position=dodge2)+
#		scale_y_continuous(name="")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		scale_y_continuous(name="")+scale_x_discrete(name="")+guides(fill=FALSE)+



		theme(axis.text.y = element_text(size=25, hjust=0.5,angle=0,color="black"), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=15, angle=0,hjust= 0.5))+
#		scale_fill_grey(start = 1, end = 0)+
		scale_fill_manual("Groups", breaks = levels(factor(statRevPeakCor$GroupName.iv)), values= c("white","#D2D2D2","#6E6E6E","#282828") )+
		scale_colour_manual("Groups", breaks = levels(factor(statRevPeakCor$GroupName.iv)), values= c("black","black","black","black") )
#		annotate(geom="text",x=0.5,y=-360,label = "(p > 0.3, Two way ANOVA)",size=5,fontface="plain",color="black",hjust=0)	
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)  
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.RevPeakCor= plot.RevPeakCor+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
#plot.pre70 = plot.pre70+layer(data=els,mapping=aes(x=GroupName.pre70,y=Peak.pre70),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+ 
plot.RevPeakCor = plot.RevPeakCor+layer(data=ivna,mapping=aes(x=GroupName.iv,y=RevPeakCor.iv),geom="point", width=1,size=3,color="black",position=position_jitter(w = 0.05, h = 0.05))
#		theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
plot.RevPeakCor = plot.RevPeakCor+#theme_bw()+
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
coord_cartesian(ylim=c(-40, 40))+  
geom_vline(xintercept=0, colour="black", size=1.5)+
		theme(axis.title.x = element_blank())
#  theme(axis.line = element_line(color = 'black'))
#plot.pre70=plot.pre70+theme(panel.background = element_rect(fill='white', colour='black'))
#tiff("~/DATA/ELS/ELS_RevPeakCor",width=10,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)

#===================================================================    Ploting RevTail : for JNS    ==============================================================
dodge2 <- position_dodge(width=-0.5,height=-0.5) 
#dodge2 <- position_dodge("dodge") 
plot.RevTailCor<-qplot(GroupName.iv, avg, fill=factor(GroupName.iv), data=statRevTailCor, geom="bar",color="black",position=dodge2)+
#		scale_y_continuous(name="")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		scale_y_continuous(name="")+scale_x_discrete(name="")+guides(fill=FALSE)+



		theme(axis.text.y = element_text(size=25, hjust=0.5,angle=0,color="black"), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=15, angle=0,hjust= 0.5))+
#		scale_fill_grey(start = 1, end = 0)+
		scale_fill_manual("Groups", breaks = levels(factor(statRevTailCor$GroupName.iv)), values= c("white","#D2D2D2","#6E6E6E","#282828") )+
		scale_colour_manual("Groups", breaks = levels(factor(statRevTailCor$GroupName.iv)), values= c("black","black","black","black") )
#		annotate(geom="text",x=0.5,y=-360,label = "(p > 0.3, Two way ANOVA)",size=5,fontface="plain",color="black",hjust=0)	
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)  
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
plot.RevTailCor= plot.RevTailCor+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
#plot.pre70 = plot.pre70+layer(data=els,mapping=aes(x=GroupName.pre70,y=Peak.pre70),geom="point", width=1,size=3,color="#9999CC",position=position_jitter(w = 0.05, h = 0.05))+ 
plot.RevTailCor = plot.RevTailCor+layer(data=ivna,mapping=aes(x=GroupName.iv,y=RevTailCor.iv),geom="point", width=1,size=3,color="black",position=position_jitter(w = 0.05, h = 0.05))
#		theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))
plot.RevTailCor = plot.RevTailCor+#theme_bw()+
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
coord_cartesian(ylim=c(-40, 40))+  
geom_vline(xintercept=0, colour="black", size=1.5)+
		theme(axis.title.x = element_blank())
#  theme(axis.line = element_line(color = 'black'))
#plot.pre70=plot.pre70+theme(panel.background = element_rect(fill='white', colour='black'))
#tiff("~/DATA/ELS/ELS_RevTailCor",width=10,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)

