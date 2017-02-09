# This is for analysis and ploting of ELS data in R
rm(list=ls())
library("ggplot2")
library("sciplot")
library("stringr")
library("plyr")
library("gdata")
	#----------------------- pre70
pre70 = read.csv('~/DATA/ELS/ELS_Pre70_new.csv',header=TRUE,stringsAsFactors=FALSE);
	# remove the column "X"
pre70=pre70[,colnames(pre70)[!grepl(colnames(pre70),pattern="X")]]  
avgpre70=ddply(pre70, .(CellID), summarize, FileName=unique(FileName),GroupName=unique(GroupName),BrainRegion=unique(BrainRegion),
ExpDate=unique(ExpDate), BirthDate=unique(BirthDate), TreatTime=mean(TreatTime), RecTime=mean(RecTime), VRest = mean(VRest),
SeriesRes = mean(SeriesRes), InputRes=mean(InputRes), mTau=mean(mTau), Sweep=mean(Sweep),VCLamp=mean(VClamp), 
Peak=mean(Peak),RiseTime=mean(RiseTime), Tail=mean(Tail), TailArea=mean(TailArea),TotalArea=mean(TotalArea))
	#----------------------- pre40
pre40 = read.csv('~/DATA/ELS/ELS_Pre40_new.csv',header=TRUE,stringsAsFactors=FALSE);
pre40=pre40[,colnames(pre40)[!grepl(colnames(pre40),pattern="X")]]
avgpre40=ddply(pre40, .(CellID), summarize, FileName=unique(FileName),GroupName=unique(GroupName),BrainRegion=unique(BrainRegion),
ExpDate=unique(ExpDate), BirthDate=unique(BirthDate), TreatTime=mean(TreatTime), RecTime=mean(RecTime), VRest = mean(VRest),
SeriesRes = mean(SeriesRes), InputRes=mean(InputRes), mTau=mean(mTau), Sweep=mean(Sweep),VCLamp=mean(VClamp), 
Peak=mean(Peak),RiseTime=mean(RiseTime), Tail=mean(Tail), TailArea=mean(TailArea),TotalArea=mean(TotalArea))
	#----------------------- post70
post70 = read.csv('~/DATA/ELS/ELS_Post70_new.csv',header=TRUE,stringsAsFactors=FALSE);
post70=post70[,colnames(post70)[!grepl(colnames(post70),pattern="X")]]
avgpost70=ddply(post70, .(CellID), summarize, FileName=unique(FileName),GroupName=unique(GroupName),BrainRegion=unique(BrainRegion),
ExpDate=unique(ExpDate), BirthDate=unique(BirthDate), TreatTime=mean(TreatTime), RecTime=mean(RecTime), VRest = mean(VRest),
SeriesRes = mean(SeriesRes), InputRes=mean(InputRes), mTau=mean(mTau), Sweep=mean(Sweep),VCLamp=mean(VClamp), 
Peak=mean(Peak),RiseTime=mean(RiseTime), Tail=mean(Tail), TailArea=mean(TailArea),TotalArea=mean(TotalArea))
colnames(avgpost70)[2:18]=paste(colnames(avgpost70),rep(".post70",length(colnames(avgpre70))),sep="")[2:18]
	#----------------------Merge pre70, pre40
els = merge(avgpre70,avgpre40,by="CellID",suffixes=c(".pre70",".pre40"))
	#----------------------Merge with post70
els = merge(els,avgpost70,all.x=TRUE,by="CellID")
	#---------------------Create the additional factors for two factor ANOVA
factors=str_split_fixed(els$GroupName.pre70,"\\+",2)
els = cbind(els,"Treat_invivo"=as.character(factors[,1]),"Treat_exvivo"=as.character(factors[,2]))
	#----------------------Generate new variables
els = cbind(els,"AmpRatio"=abs(els$Tail.pre40/els$Peak.pre70))
els = cbind(els,"AreaRatio"=abs(els$TailArea.pre40/els$TotalArea.pre70))
els = cbind(els,"PostPre"=abs(els$Peak.post70/els$Peak.pre70))
expdate=strptime(els[,"ExpDate.pre70"],"%Y%m%d");
birthdate=strptime(els[,"BirthDate.pre70"],"%Y%m%d");
age = as.numeric(difftime(expdate,birthdate,units="days"))
els = cbind(els,"Age"=age)
treattime=strptime(els[,"TreatTime.pre70"],"%H%M");
rectime=strptime(els[,"RecTime.pre70"],"%H%M");
expdelay=as.numeric(difftime(rectime,treattime,units="mins"))/60
els = cbind(els,"ExpDelay"=expdelay)
	#------------------------Select cells
select = rep(1,nrow(els));
els = cbind(els,"Select"=select)
els[els[,"ExpDate.pre70"]>=20121123,"Select"]=0;
#els[els[,"VRest.pre70"]>=-40,"Select"]=0;
els = els[els[,"Select"]==1,];

	#----------------------Statistics 2-way ANOVA
anovaAmp=(aov(AmpRatio~Treat_invivo*Treat_exvivo,els))
anovaArea=(aov(AreaRatio~Treat_invivo*Treat_exvivo,els))
anovaPostPre=(aov(PostPre~Treat_invivo*Treat_exvivo,els))
#summary(stats)
#---------------------Summary data for ploting
stat.All<-ddply(els, c("GroupName.pre70"), function(df) return(c(avg.AmpRatio=mean(df$AmpRatio), sem.AmpRatio=se(df$AmpRatio), 
									    					 avg.PeakAMPA=mean(df$Peak.pre70), sem.PeakAMPA=se(df$Peak.pre70),
														 avg.TailNMDA=mean(df$Tail.pre40), sem.TailNMDA=se(df$Tail.pre40),
														 avg.InputRes=mean(df$InputRes.pre70), sem.InputRes=se(df$InputRes.pre70),
														 avg.Age=mean(df$Age), sem.Age=se(df$Age),
														 avg.ExpDelay=mean(df$ExpDelay), sem.ExpDelay=se(df$ExpDelay)
)))

	#---------------------Summary data for ploting
stat.AmpRatio<-ddply(els, c("GroupName.pre70"), function(df) return(c(avg=mean(df$AmpRatio), sem=se(df$AmpRatio), sdev=sd(df$AmpRatio), N = sum(df$AmpRatio != 0,na.rm=TRUE))))
	#---------------------Ploting NMDA /AMPA Ratio

stat.AmpRatio$GroupName.pre70 = as.factor(stat.AmpRatio$GroupName.pre70)
stat.AmpRatio$GroupName.pre70 = reorder(stat.AmpRatio$GroupName.pre70, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))

#create the barplot component
els.plot<-qplot(GroupName.pre70, avg, fill=factor(GroupName.pre70), data=stat.AmpRatio, geom="bar", position="dodge")+
		scale_y_continuous(name="NMDA/AMPA Ratio")+scale_x_discrete(name="Groups")+guides(fill=FALSE)+
		theme(axis.text.y = element_text(size=16, hjust=0.5,angle=0), axis.title.y = element_text(size=16, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=16,face="plain"), axis.title.x = element_text(size=16, angle=0,hjust= 0.5))+
		scale_fill_manual("Groups", breaks = levels(factor(stat.AmpRatio$GroupName.pre70)), values= c("#9999CC","#9999CC","#9999CC","#9999CC") )+
		annotate(geom="text",x=0.5,y=1.2,label = "Main effect of ELS (p = 0.04)",size=5,fontface="plain",color="black",hjust=0)+
		annotate(geom="text",x=0.5,y=1.13,label = "F(1,30) = 4.4, Two way ANOVA",size=5,fontface="plain",color="black",hjust=0)		
#add error bars
#first, define the width of the dodge
dodge <- position_dodge(width=0.9)  
#now add the error bars to the plot
#els.plot=els.plot+geom_errorbar(aes(ymax=avg+sem, ymin=avg-sem), data=els.stat,position="dodge")+
els.plot=els.plot+geom_linerange(aes(ymax=avg+sem, ymin=avg-sem), position=dodge,size=1)#+theme_bw()
#now add the data points to the plot
els.plot=els.plot+layer(data=els,mapping=aes(x=GroupName.pre70,y=AmpRatio,size=Tail.pre40,color=Peak.pre70),geom="point", width=1,position=position_jitter(w = 0.05, h = 0.05))+
scale_colour_gradientn(colours = rainbow(7),name="")+ 
theme(legend.title = element_text(colour="blue", size=12, face="bold",vjust=-1,angle=0))+
scale_size_continuous(name="")
#tiff("~/DATA/ELS/ELS_NARatio_2012103",width=23,height=15,units="cm",compression=c("jpeg"),bg="white",res=300)
	#-----------------End NMDA/AMPA Ratio plot












