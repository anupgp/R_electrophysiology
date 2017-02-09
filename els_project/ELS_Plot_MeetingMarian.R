# ------------  PLOT BAR GRAPH A Parameter  ---------------------------------------------------------------
# Function to plot bargraphs with data points
barpointplot = function(data,gname,pname,ytickbegin,ytickend,ytickdelta,title,titlexpos,titleypos,ylabel)
{
	bordercolor = c("black","black","black","black")[1:length(unique(data[,gname]))];
	fillcolor = c("white","grey75","grey50","grey25")[1:length(unique(data[,gname]))];
	errorbarcolor=c("grey25","grey25","grey25","grey25")[1:length(unique(data[,gname]))];
	markercolor = c("grey25","grey25","grey25","grey25")[1:length(unique(data[,gname]))];
	xaxispos = 0.2;
	pvalues = unique(data[,pname])
	yaxisbreaks = seq(from=ytickbegin,to=ytickend,by=ytickdelta);
	datapoints = data.frame(GroupName=data[,gname],Points = data[,pname]);
pavg = ddply(data,c(gname),function(x){data.frame(GroupName=unique(x[,gname]),
				Mean=mean(x[,pname],na.rm=TRUE), Sem=se(x[,pname],na.rm=TRUE))}); # dataframe containing only the average & sem
#first, define the width of the dodge
dodge <- position_dodge(width=-0.5,height=-0.5);
barplot.param<-qplot(x=GroupName, y=Mean, fill=factor(GroupName),color=factor(GroupName),data=pavg,
				geom="bar",stat="identity",position=dodge)+
		theme(axis.text.y = element_text(size=25, hjust=0.5,color="black",angle=0), axis.title.y = element_text(size=25, angle=90,vjust=0.45),
			    axis.text.x  = element_text(angle=0, hjust=0.5, color="black",size=16,face="plain"), axis.title.x = element_text(size=25, angle=0,hjust= 0.5))+
		scale_colour_manual("Group",breaks = levels(factor(pavg$GroupName)), values= bordercolor )+
		scale_fill_manual("Group",breaks = levels(factor(pavg$GroupName)), values= fillcolor )+
theme(
    					plot.background = element_blank()
   					,panel.grid.major = element_blank()
   					,panel.grid.minor = element_blank()
   					,panel.border = element_blank()
   					,panel.background = element_blank()
					,axis.text.x=element_blank()
        				,axis.ticks.x=element_blank()
        				,axis.title.x=element_blank(),
        				#,axis.title.y=element_blank(),
					legend.position="right",legend.direction="vertical"
  					)
#Add title
#barplot.param=barplot.param+ggtitle(title)+xlab(xlabel)+ylab(ylabel);
barplot.param=barplot.param+geom_text(aes(titlexpos,titleypos,label=title),angle=0,size=10,hadjust=0,vadjust=0,color="blue")
#now add the error bars to the plot
barplot.param=barplot.param+geom_errorbar(aes(ymax=Mean+Sem, ymin=Mean-Sem),width=0,size=2.5,data=pavg,
			position=dodge,color=errorbarcolor)
#now add the data points to the plot
barplot.param = barplot.param+layer(data=datapoints,mapping=aes(x=GroupName,y=datapoints$Points,shape=factor(GroupName)),
				geom="point", width=1,size=5,color=c("red"),position=position_jitter(w = 0.2, h = 0.2))+guides(shape=FALSE)+
#draws x and y axis line
scale_y_continuous(ylabel, expand=c(0,0), breaks=yaxisbreaks, labels = as.character(yaxisbreaks)) +
coord_cartesian(ylim=yaxisbreaks)+
theme(axis.ticks.length = unit(0.25, "cm"), axis.ticks= element_line(size = 1.5,color="black"))+
geom_vline(aes(xintercept=xaxispos), colour="black", size=2);
#browser()
#jpeg("~/DATA/ELS/PlotsMeetingMarian/RevPeakCorBatch2.jpg",width=15,height=15,units="cm",bg="white",res=300)
}
#Plot function to plot two seperate parameters at once with upto 4 groups
#barPntPlot2p = function(data,gname,plist,yticks,title,titleXYpos,ylabel)
#   {
#   bordercolor = c("black");
#   fillcolor = rainbow(length(unique(data[,gname])));
#   errorbarcolor= c("black");
#   markercolor = c("grey");
#   for (i in 1:length(plist))
#   {
#   datapoints = data.frame(GroupName=data[,gname],Points = data[,pname]);
#
