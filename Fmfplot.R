#=============================================================================================================
# Function to make a lineplot (mean+sem) of one parameter with two variables
# (within and between groups, mixed design) with or without data points
Fmfplot <- function(df,idvar,paramname,withinvar,groupvar,xpos,ypos,yticks,ylabels)
	{
	df.avg = Ffunperblock(df,idvar,paramname,withinvar,groupvar,"mean",na.rm=T);
	df.sem = Ffunperblock(df,idvar,paramname,withinvar,groupvar,"se",na.rm=T);
	# Add a column to contain the groupvar
	df.avg = cbind(df.avg,groups=rownames(df.avg));
	names(df.avg)[length(df.avg)] = groupvar;
	df.sem = cbind(df.sem,groups=rownames(df.sem));
	names(df.sem)[length(df.sem)] = groupvar;
	# Reshape the dataframes from wide to long
	df.avg = reshape(df.avg,direction="long",varying=list(names(df.avg)[1:length(df.avg)-1]),
		v.names=paramname,idvar=groupvar,times=names(df.avg)[1:length(df.avg)-1],timevar=withinvar);
	df.sem = reshape(df.sem,direction="long",varying=list(names(df.sem)[1:length(df.sem)-1]),
		v.names=paramname,idvar=groupvar,times=names(df.sem)[1:length(df.sem)-1],timevar=withinvar);
	df.avgsem = merge(df.avg,df.sem,by=c(groupvar,withinvar),all.x=T,sort=F,suffixes= c(".avg",".sem"));
	dfavgsem = df.avgsem;
	names(dfavgsem) = c("Groups","Withinvar","Avg","Sem");
	dfavgsem$Groups = as.factor(dfavgsem$Groups);
	dfavgsem$Groups = reorder(dfavgsem$Groups, new.order = c("CTRL+VEH","CTRL+CORT"));
#	dfavgsem$Groups = reorder(dfavgsem$Groups, new.order = c("Actual","Fit"));
	dfavgsem$Withinvar = as.numeric(dfavgsem$Withinvar);
	#--------------------------------------------------------
        #xticks = unique(df[,withinvar]);
        #xlabels = as.character(xticks)[rep(c(1,0),length(xticks))[1:length(xticks)]];
	xlabels = c("","150","","250","","350","","450");
        xticks = c(125,150,200,250,300,350,400,450);
	#xticks = c(1,2,4,6,8,10,12,14,16,18,20);
        #xlabels = c("1","","4","","8","","12","","16","","20");
        ylabels = as.character(yticks);
        markerShape = c(24,21);
        markerBg = c("grey50","black");
        markerfg = c("grey50","black");
	dodge = position_dodge(width=-0.5,height=-0.5);
	lineplot <- qplot(x=Withinvar, y=Avg,color=factor(Groups),data=dfavgsem)+
	# Add error bars (do so before geom_point so the points are on top of the error bars)
	geom_errorbar(aes(ymax=Avg+Sem,	ymin=Avg-Sem, width=0))+
	geom_line(aes(group=Groups))+
	geom_point(aes(shape=Groups, fill=Groups), size=5) +
	scale_fill_manual("Groups", breaks = levels(factor(dfavgsem$Groups)), values= c("grey50","black") )+
	scale_color_manual("Groups", breaks = levels(factor(dfavgsem$Groups)), values= c("grey50","black") )+
	scale_shape_manual("Groups", breaks = levels(factor(dfavgsem$Groups)),values=c(24,21))+
	# explicitly have sham=fillable triangle, ACCX=fillable circle #values=c(24,21)
	theme(axis.text.y = element_text(size=25, hjust=0.5,color="black",angle=0),
	axis.title.y = element_text(size=25, angle=90,vjust=0.45),
	axis.text.x  = element_text(angle=0, hjust=0.5, color="black",size=25,face="plain"),
        legend.title = element_blank(),
        legend.text = element_text(size=20,face="plain",vjust=1),
	axis.title.x = element_text(size=25, angle=0,hjust= 0.5,vjust=-0.8))+
	theme(
                                        plot.background = element_blank()
                                        ,panel.grid.major = element_blank()
                                        ,panel.grid.minor = element_blank()
                                        ,panel.border = element_blank()
                                        ,panel.background = element_blank()
                                        #,axis.title.y=element_blank()
					#,axis.title.x=element_blank()
                                        ,legend.position="none"#"top"
                                        )+
	scale_y_discrete("", expand=c(0,0),limits=yticks, labels = ylabels) +
	scale_x_discrete("", expand=c(0,0),limits=xticks,labels = xlabels) +
        coord_cartesian(xlim=c(125,475),ylim=yticks)+
        #coord_cartesian(xlim=c(0,20),ylim=yticks)+

	theme(axis.ticks.length = unit(0.25, "cm"),axis.ticks= element_line(size = 1.5,color="black"))+
	geom_vline(aes(xintercept=ypos), colour="black", size=2)+
	geom_hline(aes(yintercept=xpos), colour="black", size=2)+
	theme(axis.ticks.length = unit(0.25, "cm"),axis.ticks= element_line(size = 1.5,color="black"))
        return(list(lineplot=lineplot,df.plot=df.avgsem));
		}
