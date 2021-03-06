## Time-stamp: <2016-02-14 17:01:33 anup>
options(contrasts=c("contr.sum","contr.poly"))

options("scipen"=10);
#Sys.setenv(http_proxy="http://192.168.1.50:3128/");
# Close all open figure windows
if ( !is.null(dev.list()) )
#{dev.off(which=dev.list())}
graphics.off();
#----------------------- Remove all data from the workspace
#rm(list=ls());
#----------------------- Load the libraries
#source("~/DATA/ELS/R/loadLibraries.R");
source("~/DATA/AHP/R/Fmfplot.R"); # Fmfplot <- function(df,idvar,paramname,withinvar,groupvar,xpos,ypos,yticks,ylabels)
source("~/DATA/AHP/R/Fxyerror.R"); #Fxyerror <-  function(df,cid,xvar,yvar,wvar,bvar)
source("~/DATA/AHP/R/FlmerpowerN.R"); #FlmerpowerN  <-  function(df1,pname,lmermod,nsim=100,N){
#----------------------- load AHP data: HC-CA1
AHP_HC = read.csv('/mnt/storage/goofy/DATA/AHP/AHP_HC/AHP_HC_CA1_400_2.csv',header=TRUE,stringsAsFactors=FALSE); # HC_CA1
AHP_LO = read.csv('/mnt/storage/goofy/DATA/AHP/AHP_OFC/AHP_OFC_LO_400_2.csv',header=TRUE,stringsAsFactors=FALSE); # OFC_LO
AHP_PL = read.csv('/mnt/storage/goofy/DATA/AHP/AHP_PFC_PL/AHP_PFC_PL_400_2.csv',header=TRUE,stringsAsFactors=FALSE); # PFC_PL
AHP_IL = read.csv('/mnt/storage/goofy/DATA/AHP/AHP_PFC_IL/AHP_PFC_IL_400_2.csv',header=TRUE,stringsAsFactors=FALSE); # PFC_IL

# Merge dataframes
AHP = rbind(AHP_HC,AHP_LO,AHP_PL,AHP_IL);
#AHP = AHP_LO;
# Remove the orginal dataframes
rm(list=c("AHP_HC","AHP_LO","AHP_PL","AHP_IL"));
# remove the column "X"
AHP = AHP[,(colnames(AHP)[!grepl(colnames(AHP),pattern="X")]) ];
# add new colum FileID = FileName+CellID
AHP = cbind(AHP,FileID = paste(AHP[,"FileName"],AHP[,"CellID"],sep="_"),stringsAsFactors=F);
# Remove column FileName & CellID
AHP = AHP[,(colnames(AHP)[!(grepl(colnames(AHP),pattern="FileName") & grepl(colnames(AHP),pattern="CellID")) ]) ];
# -------------- Compute ExpDelay and Age
expdate=strptime(as.integer(AHP[,"ExpDate"]),"%Y%m%d");
birthdate=strptime(as.integer(AHP[,"BirthDate"]),"%Y%m%d");
age = as.numeric(difftime(expdate,birthdate,units="days"))
AHP = cbind(AHP,"Age"=age)
treattime=strptime(AHP[,"TreatTime"],"%H%M");
rectime=strptime(AHP[,"RecTime"],"%H%M");
expdelay=as.numeric(difftime(rectime,treattime,units="mins"))/60
AHP = cbind(AHP,"ExpDelay"=expdelay)
# ------------- Remove the averaged trace from the dataframe
AHP = AHP[AHP$Sweep != 0,];
#-------------- Order Between group factor levels
AHP[,"GroupName"] = as.factor(AHP[,"GroupName"]);
AHP[,"GroupName"] = reorder(AHP[,"GroupName"], new.order= c("CTRL+VEH","CTRL+CORT"));
# ------------- Compute average passive properties: Input resistance, series resistance and membrane Tau
psvavg = ddply(AHP,c("FileID"),function(x){data.frame(AvgInputResf = rep(mean(x[x$Count==1,"InputResf"],na.rm=T),dim(x)[1]),
         AvgInputResa = rep(mean(x[x$Count==1,"InputResa"],na.rm=T),dim(x)[1]), AvgSeriesRes = rep(mean(x[x$Count==1,"SeriesRes"],na.rm=T),     dim(x)[1]),
         AvgTauMem = rep(mean(x[x$Count==1,"TauMem"],na.rm=T),dim(x)[1]),AvgBasePreAhp = rep(mean(x[x$Count==1,"BasePreAhp"],na.rm=T),dim(x)[1]),
         FileID = x[,"FileID"],IClamp=x[,"IClamp"],Count=x[,"Count"],GroupName = x[,"GroupName"]  )});
#-------------- Merge the dataframes
AHP = merge(AHP,psvavg,by=c("FileID","GroupName","IClamp","Count"),all=T,sort=F);
# ------------- Compute spike accomodation
Acco1 = ddply(AHP,c("FileID","IClamp"),function(x){
      	if (length(x[,"ApTime"]) > 1) {data.frame(SpkInt = c(0,diff(x[,"ApTime"],lag=1))*1000,
	SpkFreq = c(0,1/diff(x[,"ApTime"],lag=1)), AvgSpkInt = rep(mean(diff(x[,"ApTime"],lag=1),na.rm=T),dim(x)[1])*1000,
	AvgSpkFreq = rep(mean((1/diff(x[,"ApTime"],lag=1)),na.rm=T),dim(x)[1]),
	SpkIntLast = rep(diff(x[,"ApTime"],lag=1)[length(diff(x[,"ApTime"],lag=1))],length(x[,"ApNum"]))*1000,
	SpkIntFirst = rep(diff(x[,"ApTime"],lag=1)[1],length(x[,"ApNum"]))*1000,
        # SpkIntLF in percents (%)
      SpkIntLF = rep((diff(x[,"ApTime"],lag=1)[length(diff(x[,"ApTime"],lag=1))]/diff(x[,"ApTime"],lag=1)[1]),length(x[,"ApNum"]))*10,
        ApTotal = rep(sum(x[,"ApNum"] != 0)),
	FileID = x[,"FileID"],GroupName = x[,"GroupName"],IClamp=x[,"IClamp"],Count=x[,"Count"], ApNum=x[,"ApNum"] )}
	else {data.frame(SpkInt=0,SpkFreq= 0, AvgSpkInt=0, AvgSpkFreq=0,SpkIntLast = 0,ApTotal=0,
	FileID = x[,"FileID"],GroupName = x[,"GroupName"],IClamp=x[,"IClamp"], Count=x[,"Count"], ApNum=x[,"ApNum"] )} })
AHP = merge(AHP,Acco1,by=c("FileID","GroupName","IClamp","Count","ApNum"),all=T,sort=F);
Acco2 = ddply(AHP,c("FileID"),function(x){
			data.frame(ApMax= max(x[,"ApTotal"]),
			FileID = x[,"FileID"],GroupName = x[,"GroupName"],IClamp=x[,"IClamp"],Count=x[,"Count"])
			 })
AHP = merge(AHP,Acco2,by=c("FileID","GroupName","IClamp","Count"),all=T,sort=F);
rm(list=c("Acco1","Acco2","psvavg","age","birthdate","expdate","expdelay","rectime","treattime"));
# ------------- Add new columns for selection criteria
AHP = cbind(AHP,"Select"=1,"SelectBad"=1,"SelectIR"=1,"SelectSR"=1,"SelectAp"=1);
#============================================== FUNCTIONS ================================================
# Fmfplot,Fmaketable,Fparamttest,Ffunperblock,Famelia,Ffindmissing,Fsdoutliers,Favgparam,Fcountcells,Frepcol,Freprow,Fanovamix
# Function to fixWeirdAnovaBehavior
Fanovamix <- function(df,idvar,paramname,withinvar,groupvar)
	{
	df = df[,c(idvar,withinvar,groupvar,paramname)];
	names(df) = c("idvar","withinvar","groupvar","paramname");
	anova.int = anova(lmer(paramname ~ groupvar*withinvar + (1 + withinvar | idvar),data=df,REML=FALSE),
	 (lmer(paramname ~ groupvar + withinvar + (1 + withinvar | idvar),data=df,REML=FALSE)));
        sig.int = anova.int[2,"Pr(>Chisq)"];
	anova.mig = anova(lmer(paramname ~ groupvar*withinvar + (1 + withinvar | idvar),data=df,REML=FALSE),
	(lmer(paramname ~ groupvar:withinvar + withinvar + (1 + withinvar | idvar),data=df,REML=FALSE)));
        # The original model specification is below
        #(lmer(paramname ~ (groupvar*withinvar) + (1 + withinvar | idvar ),data=df))
        print(anova.mig);
        print(anova.int);
        sig.mig = anova.mig[2,"Pr(>Chisq)"];
	print(paste(" Signifiance for interaction = ", sig.int));
	print(paste(" Signifiance for main effect for groups = ", sig.mig));
        return(list(main.sig=as.matrix(anova.mig$"Pr(>Chisq)")[2,1],int.sig=as.matrix(anova.int$"Pr(>Chisq)")[2,1]));
	}
Freprow <- function(x,n){matrix(rep(x,each=n),nrow=n)}; # Repates Rows
Frepcol <- function(x,n){matrix(rep(x,each=n), ncol=n, byrow=TRUE)}; # Repeates Columns
Fcountcells <- function(x){sum(!is.na(x))} # Function to count the number of cells that are not NA
#=============================================================================================
# Function returns dataframe of averaged values of a list of parameters for the particular levels of a within factor for each cell.
Favgparam <- function(df,idvar,paramname,withinvar,groupvar,avglevels)
	{
	ddplyfun <- function(x,paramname,withinvar,groupvar,avglevels)
		{
		data.frame(Param=rep(mean(x[,paramname][x[,withinvar]%in%avglevels],na.rm=T),length(avglevels)),
				Idvar = rep(unique(x[,idvar]),length(avglevels)),
				Groupvar = rep(unique(x[,groupvar]),length(avglevels)),
				Index = 1:length(avglevels))
		}
	avgdf = ddply(df,c(idvar),ddplyfun,paramname,withinvar,groupvar,avglevels)
	avgdf = avgdf[avgdf$Index == 1,];
	avgdf = avgdf[,c("Idvar","Groupvar","Param")];
	names(avgdf)=c(idvar,groupvar,paramname);
	return(avgdf);
	}
#==========================================================================================
# Function to remove outliers 'n' times Sdev == NA in a two factor mixed design dataframe
Fsdoutliers <- function(df,idvar,paramname,withinvar,groupvar,sdtimes)
   {
   ddplyfun.OutlierNA <- function(x,idvar,withinvar,groupvar,paramname,sdtimes)
      {
      dummy_var= rep(NA,dim(x)[1]);
      y = x[,paramname];
      indx_yin = which( y >= (mean(y,na.rm=T)-(sdtimes*sd(y,na.rm=T))) & y <= (mean(y,na.rm=T) + (sdtimes*sd(y,na.rm=T))) );
      dummy_var[indx_yin] = y[indx_yin];
      df=data.frame(Param =  dummy_var,Idvar = x[,idvar],Withinvar = x[,withinvar],Groupvar = x[,groupvar] );
      return(df);
      }
      df.out = ddply(df,c(groupvar,withinvar),ddplyfun.OutlierNA,idvar,withinvar,groupvar,paramname,sdtimes);
      df.out = df.out[,c("Idvar","Groupvar","Withinvar","Param")];
      names(df.out)=c(idvar,groupvar,withinvar,paramname);
      return(df.out)
   }
#=======================================================================================
# Function that detects and records NA for cell/block with missing values
Ffindmissing <- function(df,idvar,paramname,withinvar,groupvar)
   {
   levels.all = levels(as.factor(df[,withinvar]));
   ddplyfun.missingfind <- function(x,paramname,withinvar,groupvar,levels.all)
      {
      val = rep(NA, length(levels.all));val[which(levels.all%in%x[,withinvar])] = x[,paramname];
      data.frame(Param=val, Idvar=rep(unique(x[,idvar]),length(levels.all)),Groupvar = rep(unique(x[,groupvar]),length(levels.all)),Withinvar=levels.all)
      }
      df.full = ddply(df,idvar,ddplyfun.missingfind,paramname,withinvar,groupvar,levels.all);
      df.full = df.full[,c("Idvar","Groupvar","Withinvar","Param")];
      names(df.full)=c(idvar,groupvar,withinvar,paramname);
      return(df.full);
   }
#=======================================================================================
# Function that imputes the dataframe "n" times if missing values present and averages all the imputed values to return one complete dateset.
Famelia <- function(df,idvar,paramname,withinvar,groupvar,m,...)
   {
   ddplyfun.ameliami <- function(x,idvar,withinvar,groupvar,paramname)
      {
      data.frame(Param=mean(x[,paramname]), Idvar=unique(x[,idvar]),Groupvar = unique(x[,groupvar]),Withinvar=unique(x[,withinvar]) )
      }
   print("Finding the Missing values for the within factor levels");
   df = Ffindmissing(df,idvar,paramname,withinvar,groupvar);
   colnum.param = which(names(df)%in%paramname);
   if (is.factor(df[,withinvar]))
      {
      print("Within group variable is a factor, converting to numeric....");
      df[,withinvar] = as.numeric(as.character(df[,withinvar]));
      }
   print("No ouliers removed");
   bounds = matrix(c(colnum.param,min(df[,paramname]),max(df[,paramname]) ),1,3)
   ls.MI = amelia(x=df,m=m,ts=withinvar,cs=groupvar,idvars=idvar,polytime=3,intercs=T,bounds=bounds,p2s=0);
   df.MIraw = ldply(ls.MI$imputations);
   df.MIavg  = ddply(df.MIraw,c(idvar,withinvar),ddplyfun.ameliami,idvar,withinvar,groupvar,paramname);
   df.final = df.MIavg[,c("Idvar","Groupvar","Withinvar","Param")];
   names(df.final) = c(idvar,groupvar,withinvar,paramname);
   #mixanova=Fanovamix(df.final,idvar,paramname,withinvar,groupvar);
   return(as.data.frame(df.final));
   }
#----------------------------------------------------------------------------------------------------
# Function computes an operation on each cell block of the design (two factors, also mixed)
Ffunperblock <- function(df,idvar,paramname,withinvar,groupvar,funname,...)
	{
	levels.withinvar = unique(as.character(df[,withinvar]));
	levels.groupvar = unique(as.character(df[,groupvar]));
	#mat.df=matrix(NA,length(levels.groupvar),length(levels.withinvar));
	mat.df=as.data.frame(matrix(NA,length(levels.groupvar),length(levels.withinvar)),row.names=levels.groupvar);
	names(mat.df) = levels.withinvar;
	i=1;
	FUN = get(funname);
	while( i<= length(levels.groupvar) ) # Between subject loop: GroupName
		{
		j =1;
		while( j <= length(levels.withinvar) ) # Within subject loop: IClamp
			{
			mat.df[levels.groupvar[i],levels.withinvar[j]]= FUN(df[ (df[,groupvar] == levels.groupvar[i] & df[,withinvar] == levels.withinvar[j]),paramname],...);
			j = j+1;
			}
		i = i+1;
		}

	return(mat.df);
	}
Fparamttest <- function(df,param,groups){test=t.test(get(param) ~ get(groups),paired=F,data=df); return(test$"p.value")}
#-----------------------------------------------------------------------------------------------------------
# Function creates a table with p-values for all the parameters passed
Fmaketable <- function(df,idvar,paramnames,withinvar,groupvar,funname)
   {
   #Genreate the matrix
   out.mat = matrix(NA,length(levels(as.factor(df[,groupvar])))+1,length(paramnames))
   df.out = as.data.frame(out.mat);
   names(df.out)=paramnames;
   rownames(df.out)= c(levels(as.factor(df[,groupvar])),"Sig");
   for (item in paramnames)
   {
      aggdata1=aggregate(get(item) ~ (get(idvar)+get(groupvar)),FUN=mean,df)
      names(aggdata1)=c(idvar,groupvar,item);
      #browser();
      aggdata2 = aggregate(get(item) ~ get(groupvar),FUN=funname,data=aggdata1);
      names(aggdata2)= c(groupvar,item);
      df.out[1:length(levels(as.factor(df[,groupvar]))),item] = aggdata2[,item];
      df.out["Sig",item] =  Fparamttest(aggdata1,item,groupvar);
      #browser();
   }
   return(df.out);
   }
#========================================
#Function to extract SEs from lmer fixed effects estimates
se.fixef <- function(lmer.mod)
   {
   lmer.names= names(fixef(lmer.mod))
   ses = sqrt(diag(vcov(lmer.mod,useScale=FALSE)));
   names(ses)=lmer.names;
   return(ses);
   }
#============================================================
#Function to calculate power of mixed factors model with random slope and intercepts
Flmerpower = function(lmerdf,lmerFmod,nsim)
   {
   # make dataframe to hold the significance for anova for main effects & interaction
   lmeranova = data.frame(MEGroupName=rep(NA,nsim),Inter=rep(NA,nsim));
   lmerseed=2014;
   #Simulate nSim
   simdfbig=cbind(lmerdf,simulate(lmerFmod,nsim=nsim,seed=lmerseed,use.u=T));# new simulated column name = sim_1
   for (i in 1:nsim)
      {
      param = paste("sim_",as.character(i),sep="");
      simdfsmall = simdfbig[,c("FileID",param,"IClamp","GroupName")];
      mfanova = Fanovamix(simdfsmall,"FileID",param,"IClamp","GroupName");
      lmeranova[i,"MEGroupName"]=mfanova$main.sig;
      lmeranova[i,"Inter"]=mfanova$int.sig;
      rm(simdfsmall);
      }
   lmerpower = list(power.main=sum(lmeranova$MeEGroupName<0.05),power.int=sum(lmeranova$Inter<0.05));
   return(list(power=lmerpower,pvalues=lmeranova));
   }
#==============================================================
# Do lmer in function mode
Flmer<-function(dflmer,IDVar,ParamName,WithinVar,GroupVar)
   {
   dflmer = dflmer[,c(IDVar,ParamName,WithinVar,GroupVar)];
   names(dflmer) = c("idvar","paramname","withinvar","groupvar");
   lmermod = lmer(paramname ~ groupvar*withinvar + (1 + withinvar | idvar),data=dflmer,REML=FALSE);
   return(lmermod);
   }
#=======================================================================================
FoutlierAmeliaAov <- function(df1,IDVar,ParamName,WithinVar,GroupVar,...)
   {
   # Step 1: Make lmer model with orginal data set. No data point is removed
   # Step 2: Perform outlier test with "influence". Use Cooks distance>4/n, n= no.of data points, to identify the
   #  influencing/outlying data points
   # Step 3: Delete the identified influencing/outlying data points
   # Step 4: Perform Amelia on to impute the missing data points
   # Step 5: Make new model
   # Step 6: Repeat Step 2 - Step 5, until no outliers detected.
   # Step 7: Perfom anova to obtain significance for main effects and interaction.
   df1 = df1[,c(IDVar,ParamName,WithinVar,GroupVar)];
   namesdf1=names(df1) = c("idvar","paramname","withinvar","groupvar");
   #------------
   maxcount=5
   count=1
   nas=1;
   ameliaruns=100;
   df2=df1;
   while(count<=maxcount)
      {
      print(c("No. of outlier detection runs = ",as.character(count)));
      lmermod =	lmer(paramname ~ groupvar*withinvar + (1 + withinvar | idvar),data=df2,REML=FALSE);
      print("running influence");
      ae.lmermod=influence(lmermod,obs=TRUE);
      print("removing the datapoints using Cook's distance");
      #browser();
      dfnew = cbind(df2,"cooksd"=cooks.distance(ae.lmermod));
      dfnew[dfnew[,"cooksd"]>(4/nrow(dfnew)),"paramname"]=NA;
      df2 = dfnew[,namesdf1];rm(dfnew);
      names(df2) = c(IDVar,ParamName,WithinVar,GroupVar);
      nas=sum(is.na(df2[,ParamName]))
      print(c("no. of NA's = ",as.character(nas)));
      if(nas==0 & count>1)
         {
         print("No. NAs =0 & Count>1: BREAK");
         break;
         }
      #browser();
      if(count==maxcount)
         {
         ameliaruns=500;
         }
      if(nas!=0 | count <=1)
         {
         print("Running Amelia");
         df2 = Famelia(df2,IDVar,ParamName,WithinVar,GroupVar,ameliaruns);
         df2 = df2[,c(IDVar,ParamName,WithinVar,GroupVar)];
         names(df2) = c("idvar","paramname","withinvar","groupvar");
         }
      count=count+1;
      if(count > maxcount)
         {
         print("exceeded the maximum number of counts");
         break;
         }
      }
      names(df2) = c(IDVar,ParamName,WithinVar,GroupVar);
   return(df2);
   }
#==============================================================================================================
#jpeg("~/DATA/AHP/AHP_OFC/Plots/LinePeakAhpSf.jpg",width=12,height=12,units="cm",bg="white",res=300)

#--------------------------------------------------------------------------------------------------------------
# Function to make a lineplot (mean+sem) of one parameter with two variables
# (within and between groups, mixed design) with or without data points
# lineplotMixedFactor <- function(df,idvar,withinvar,groupvar,paramname,ytickbegin,ytickend,ytickdelta)
#	{
#	markerbordercolor = c("black","black","black","black")[1:length(unique(df[,groupvar]))];
#	markerfillcolor = c("black","black","black","black")[1:length(unique(df[,groupvar]))];
#	df.avg = dfFunPerBlock(df,idvar,paramname,withinvar,groupvar,"mean");
#	df.sem = dfFunPerBlock(df,idvar,paramname,withinvar,groupvar,"se");
#	# Add a column to contain the groupvar
#	df.avg = cbind(df.avg,groups=rownames(df.avg));
#	names(df.avg)[length(df.avg)] = groupvar;
#	df.sem = cbind(df.sem,groups=rownames(df.sem));
#	names(df.sem)[length(df.sem)] = groupvar;
#	# Reshape the dataframes from wide to long
#	df.avg = reshape(df.avg,direction="long",varying=list(names(df.avg)[1:length(df.avg)-1]),
#		v.names=paramname,idvar=groupvar,times=names(df.avg)[1:length(df.avg)-1],timevar=withinvar);
#	df.sem = reshape(df.sem,direction="long",varying=list(names(df.sem)[1:length(df.sem)-1]),
#		v.names=paramname,idvar=groupvar,times=names(df.sem)[1:length(df.sem)-1],timevar=withinvar);
#	df.avgsem = merge(df.avg,df.sem,by=c(groupvar,withinvar),all.x=T,sort=F,suffixes= c(".avg",".sem"));
#	dodge = position_dodge(width=-0.5,height=-0.5);
#	barplot.param <- qplot(x=get(withinvar), y=get(paste(paramname,".avg",sep="")),
#		fill=factor(get(groupvar)),color=factor(get(groupvar)),data=df.avgsem)+
#	geom_errorbar(aes(ymax=get(paste(paramname,".avg",sep=""))+get(paste(paramname,".sem",sep="")),
#		ymin=get(paste(paramname,".avg",sep=""))-get(paste(paramname,".sem",sep="")),width=0))+
#	geom_line()+geom_point(size=5)
#	}
#==============================================================================================================
#lmer(Param ~ IClamp+ (GroupName:IClamp) + (1+IClamp|FileID),data=ahpsmall

