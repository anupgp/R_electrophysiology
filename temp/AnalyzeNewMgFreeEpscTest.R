#installed.packages()
#rm(list=ls());
# =================================================== LOADING EPSCs =========================================================
#----------------------LOAD PRE70
MgFreeEPSC = read.csv("~/DATA/ELS/CSV/EvokedEpsc_MgFree.csv",header=TRUE,stringsAsFactors=FALSE,row.names=NULL);
# ============================================= REMOVE/CHANGE  COLUMNS/VALUES  ================================================
MgFreeEPSC=MgFreeEPSC[,colnames(MgFreeEPSC)[!grepl(colnames(MgFreeEPSC),pattern="X")]]; # REMOVE THE COLUMN "X"
# Remove the IV columns from EPSC "EPSC-70"          "EPSC-70CNQMgFree" "EPSC+40"
MgFreeEPSC = MgFreeEPSC[,!names(MgFreeEPSC)%in%c("RevPeakCor","RevPeakUncor","RevTailCor","RevTailUncor")];
MgFreeEPSC=MgFreeEPSC[MgFreeEPSC$Sweep != -1,]; # REMOVE THE AVERAGED SWEEP
MgFreeEPSC[MgFreeEPSC[,"TauPeak"] >1 | MgFreeEPSC[,"TauPeak"] < 0,"TauPeak"] = NA; # REMOVE BAD TAU FROM EPSC PEAK FIT
MgFreeEPSC[MgFreeEPSC[,"TauTail"] >1 | MgFreeEPSC[,"TauTail"] < 0,"TauTail"] = NA; # REMOVE BAD TAU FROM EPSC TAIL FIT
MgFreeEPSC[MgFreeEPSC[,"VRest"] > -50,"VRest"]  = NA; # REMOVE VALUES OF VREST THAT IS ZERO TO NA
# ================================================== ADD COLUMNS =========================================================
MgFreeEPSC = cbind(MgFreeEPSC,Select=1); #  MAKE A NEW COLUMN TO INDICATE SELECTED CELLS
MgFreeEPSC = ddply(MgFreeEPSC,c("CellID","RecMode"),function(x){as.data.frame(cbind(x,Count = 1:length(x[,1]))) }); # ADD COUNT COLUMN FOR SWEEP COUNT
# ADD A TIMEINDEX COLUMN: 0 = LAST PRE70 SWEEP, 1 = FIRST PRE40 SWEEP, ONLY TAKE A MAXIMUM OF 20 SWEEPS FROM PRE40
MgFreeEPSC = ddply(MgFreeEPSC,c("CellID","RecMode"),function(x){
	if(grepl(unique(x[,"RecMode"]),pat="EPSC-70")) {first = -(length(x[,1])-1); last=0; firstrow=1; lastrow=length(x[,1]);}
	if(grepl(unique(x[,"RecMode"]),pat="EPSC-70CNQMgFree")) {first=1; last=min(length(x[,1]),20); firstrow=1; lastrow=min(length(x[,1]),20);}
	if(grepl(unique(x[,"RecMode"]),pat="EPSC\\+40")) {first=21; last=(21+length(x[,1])-1); firstrow=1;lastrow=length(x[,1]);}
	as.data.frame(cbind(x[firstrow:lastrow,],TimeIndex=seq(first,last,by=1))) });
# ADD A NEW COLUM TIME = TIMEINDEX * (25/60)
MgFreeEPSC = cbind(MgFreeEPSC,Time=MgFreeEPSC[,"TimeIndex"]*25/60);
# ADD NEW COLUMN FOR AGE OF THE ANIMAL
MgFreeEPSC = cbind(MgFreeEPSC,Age=as.numeric(difftime(strptime(MgFreeEPSC[,"ExpDate"],"%Y%m%d"),strptime(MgFreeEPSC[,"BirthDate"],"%Y%m%d"),units="days")) );
# ADD NEW COLUMN FOR AGE OF EXPERIMENTAL DELAY
MgFreeEPSC = cbind(MgFreeEPSC,ExpDelay=as.numeric(difftime(strptime(MgFreeEPSC[,"RecTime"],"%H%M"),strptime(MgFreeEPSC[,"TreatTime"],"%H%M"),units="mins"))/60 );
# ADD NEW COLUMN FOR MEMBRANE CAPACITANCE
MgFreeEPSC = cbind(MgFreeEPSC,MemCap = (1/MgFreeEPSC$SeriesRes + 1/MgFreeEPSC$InputRes)*MgFreeEPSC$TauMem*1E06 ); # UNITS= PICO FARADS
# ADD NEW COLUMN FOR CVs
MgFreeEPSC = ddply(MgFreeEPSC,c("CellID","RecMode"),function(x){as.data.frame(cbind(x,CVPeak = (var(x[,"Peak"])-var(x[,"Noise"]) )/(mean(x[,"Peak"])^2),
      CVTail =  (var(x[,"Tail"])-var(x[,"Noise"]) )/(mean(x[,"Tail"])^2)	))});
#============================================================ ACTIVATE THE SELECTION =======================================
#MgFreeEPSC = MgFreeEPSC[MgFreeEPSC$Select==1,];
#================================================== ORDER THE LEVELS OF GROUPNAME ========================================
MgFreeEPSC$GroupName = as.factor(MgFreeEPSC$GroupName);
MgFreeEPSC$GroupName = reorder(MgFreeEPSC$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"));
#============================================================================================================================
# FIND THE NUMBER OF SWEEPS FOR EACH EPOCH FOR ALL THE CELLS
mgfreeepsccells = unique(MgFreeEPSC[,"CellID"]); # GET ALL THE CELLS RECORDED FOR EPSC
nrecmode = length(levels(as.factor(MgFreeEPSC$RecMode)));
recmodes = levels(as.factor(MgFreeEPSC$RecMode));
mgfreeepscsweepsmat = matrix(NA,length(unique(MgFreeEPSC[,"CellID"])),length(unique(MgFreeEPSC[,"RecMode"])));
i=1;
for (i in 1:length(mgfreeepsccells))
{
j=1;
for (j in 1:length(recmodes))
{mgfreeepscsweepsmat[i,j] = length(MgFreeEPSC[grepl(MgFreeEPSC$CellID,pat=mgfreeepsccells[i],fixed=T) & grepl(MgFreeEPSC$RecMode,pat=recmodes[j],fixed=T),"Peak"]) };
}
MgFreeEPSCSweeps=as.data.frame(mgfreeepscsweepsmat, row.names=NULL);
colnames(MgFreeEPSCSweeps)=levels(as.factor(MgFreeEPSC$RecMode));
##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Normalize each cell with it's baseline and add a column with averaged LTP
ltpavgtimestart = 13;
ltpavgtimestop = 16; # Average the last 5 min of Post70 epoch
MgFreeEPSC = ddply(MgFreeEPSC,c("CellID"),function(x){
			#------------ AVERAGE VALUE AT PRE70
			PkEpsc70=mean(x[grepl(x$RecMode,pat="EPSC-70",fixed=T),"Peak"]);
			PkEpsc70CnqMgFree=mean(x[grepl(x$RecMode,pat="EPSC-70CNQMgFree",fixed=T),"Peak"]);
                        PkEpsc40=mean(x[grepl(x$RecMode,pat="EPSC+40",fixed=T),"Peak"]);
                        TlEpsc40=mean(x[grepl(x$RecMode,pat="EPSC+40",fixed=T),"Tail"]);

#------------ LENGTHS OF EPOCHS
			nEpsc70=length(x[grepl(x$RecMode,pat="EPSC-70",fixed=T),"Peak"]);
			nEpsc70CnqMgFree=length(x[grepl(x$RecMode,pat="EPSC-70CNQMgFree",fixed=T),"Peak"]);
                        nEpsc40=length(x[grepl(x$RecMode,pat="EPSC+40",fixed=T),"Peak"]);
                       	#------------- normalized values for plotting LTP/potentiation after NMDA activation
			data.frame(cbind(x,
			NPeak = (x[,"Peak"]/PkEpsc70)*100,
			#-----------
			AvgPkEpsc70 =  mean(x[grepl(x$RecMode,pat="EPSC-70",fixed=T),"Peak"]),
			AvgPkEpsc70CnqMgFree =  mean(x[grepl(x$RecMode,pat="EPSC-70CNQMgFree",fixed=T),"Peak"]),
			AvgTailPre40 =  mean(x[grepl(x$RecMode,pat="Epsc40"),"Tail"]),
			AvgNPeakPre40 = 	mean(x[grepl(x$RecMode,pat="PRE40"),"Peak"])/abs(peakpre70),
			AvgNTailPre40 = 	mean(x[grepl(x$RecMode,pat="PRE40"),"Tail"])/abs(peakpre70),
			#----------
			NARatioPeak = mean(x[grepl(x$Epoch,pat="PRE40"),"Tail"])/ peakpre70,
			NARatioArea = mean(x[grepl(x$Epoch,pat="PRE40"),"AreaTail"])/ areapeakpre70,
			#------------------------ Passive properties averaged from the PRE70
			AvgSeriesRes =  mean(x[grepl(x$Epoch,pat="PRE70"),"SeriesRes"],na.rm=T),
			AvgInputRes =   mean(x[grepl(x$Epoch,pat="PRE70"),"InputRes"],na.rm=T),
			AvgMemCap = mean(x[grepl(x$Epoch,pat="PRE70"),"MemCap"],na.rm=T),
			#------------------------ General properties of the EPSCs for AMPA(Pre70) & NMDA(PRE40)
			AvgRiseTimePre70 = mean(x[grepl(x$Epoch,pat="PRE70"),"RiseTime"],na.rm=T),
			AvgTauPeakPre70 = mean(x[grepl(x$Epoch,pat="PRE70"),"TauPeak"],na.rm=T),
			AvgSlopeFitPre70 = mean(x[grepl(x$Epoch,pat="PRE70"),"SlopeFit"],na.rm=T),
			CVPeakPre70 = sqrt(var(x[grepl(x$Epoch,pat="PRE70"),"Peak"],na.rm=T)-var(x[grepl(x$Epoch,pat="PRE70"),"Noise"],na.rm=T) ) /
																				 mean(x[grepl(x$Epoch,pat="PRE70"),"Peak"],na.rm=T),
			#------------------------
			AvgRiseTimePre40 = mean(x[grepl(x$Epoch,pat="PRE40"),"RiseTime"],na.rm=T),
			AvgTauPeakPre40 = mean(x[grepl(x$Epoch,pat="PRE40"),"TauPeak"],na.rm=T),
			AvgTauTailPre40 = mean(x[grepl(x$Epoch,pat="PRE40"),"TauTail"],na.rm=T),
			AvgSlopeFitPre40 = mean(x[grepl(x$Epoch,pat="PRE40"),"SlopeFit"],na.rm=T),
			CVTailPre40 = sqrt(var(x[grepl(x$Epoch,pat="PRE40"),"Peak"],na.rm=T)-var(x[grepl(x$Epoch,pat="PRE40"),"Noise"],na.rm=T) ) /
																				mean(x[grepl(x$Epoch,pat="PRE40"),"Peak"],na.rm=T),
			#-----------------------
			CVPeakPost70 = sqrt(var(x[grepl(x$Epoch,pat="POST70"),"Peak"],na.rm=T)-var(x[grepl(x$Epoch,pat="POST70"),"Noise"]) )/
																				mean(x[grepl(x$Epoch,pat="POST70"),"Peak"],na.rm=T),
			#------------------------
			Invivo = strsplit(unique(as.character(x[,"GroupName"])),split="+",fixed=T)[[1]][1],
			Exvivo = strsplit(unique(as.character(x[,"GroupName"])),split="+",fixed=T)[[1]][2]
			)
			)});
#browser();
#==================================================== CELL SELECTION CRITERIA  ==--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
