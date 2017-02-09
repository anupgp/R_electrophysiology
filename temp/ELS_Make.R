#installed.packages()
rm(list=ls());
# =================================================== LOADING EPSCs =========================================================
PRE70 = read.csv('~/DATA/ELS/CSV/pre70new.csv',header=TRUE,stringsAsFactors=FALSE,row.names=NULL); #----------------------LOAD PRE70
PRE40 = read.csv('~/DATA/ELS/CSV/pre40new.csv',header=TRUE,stringsAsFactors=FALSE,row.names=NULL); #--------------------LOAD PRE40
POST70 = read.csv('~/DATA/ELS/CSV/post70new.csv',header=TRUE,stringsAsFactors=FALSE,row.names=NULL); #--------------------LOAD POST70

# =============================================== LOADING IV RELATIONSHIPS ====================================================
IV = read.csv('~/DATA/ELS/CSV/ELS_IVnew.csv',header=TRUE,stringsAsFactors=FALSE,row.names=NULL); #----------------------- LOAD IV

# ============================================ LOADING TEMPORAL/SUPER SUMMATION ==================================================
#SS70100 = read.csv('~/DATA/ELS/CSV/ELS_ss70100.csv',header=TRUE,stringsAsFactors=FALSE,row.names=NULL);#-------------------- SS70100
#SS7050 = read.csv('~/DATA/ELS/CSV/ELS_ss7050.csv',header=TRUE,stringsAsFactors=FALSE,row.names=NULL);#------------------------ SS7050
#SS40100 = read.csv('~/DATA/ELS/CSV/ELS_ss40100.csv',header=TRUE,stringsAsFactors=FALSE,row.names=NULL);#-------------------- SS40100
#SS4050 = read.csv('~/DATA/ELS/CSV/ELS_ss4050.csv',header=TRUE,stringsAsFactors=FALSE,row.names=NULL);#------------------------ SS4050

# ================================================== COMBINE =========================================================
EPSC = rbind(cbind(PRE70,Epoch="PRE70"),cbind(PRE40,Epoch="PRE40"),cbind(POST70,Epoch="POST70")); # COMBINE PRE70, PRE40 & POST70
#SS = rbind(SS70100,SS7050,SS40100,SS4050); # COMBINE SS70100, SS7050, SS40100 & SS4050

# ============================================= REMOVE/CHANGE  COLUMNS/VALUES  ================================================
EPSC=EPSC[,colnames(EPSC)[!grepl(colnames(EPSC),pattern="X")]]; # REMOVE THE COLUMN "X"
EPSC = EPSC[,!names(EPSC)%in%c("RevPeakCor","RevPeakUncor","RevTailCor","RevTailUncor")]; # Remove the IV columns from EPSC
EPSC=EPSC[EPSC$Sweep != -1,]; # REMOVE THE AVERAGED SWEEP
EPSC[EPSC[,"TauPeak"] >1 | EPSC[,"TauPeak"] < 0,"TauPeak"] = NA; # REMOVE BAD TAU FROM EPSC PEAK FIT
EPSC[EPSC[,"TauTail"] >1 | EPSC[,"TauTail"] < 0,"TauTail"] = NA; # REMOVE BAD TAU FROM EPSC TAIL FIT
EPSC[EPSC[,"VRest"] > -50,"VRest"]  = NA; # REMOVE VALUES OF VREST THAT IS ZERO TO NA
#----------------------
#IV=IV[,colnames(IV)[!grepl(colnames(IV),pattern="X")]]; # REMOVE THE COLUMN "X"
#IV Peak & Tail Normalize
#IV = ddply(IV,c("CellID"),function(x){data.frame(cbind(x,Peakn=x[,"Peak"]/x[,"Peak"][length(x[,"Peak"])],Tailn = x[,"Tail"]/x[,"Tail"][length(x[,"Tail"])]))});
#-----------------------
#SS=SS[,colnames(SS)[!grepl(colnames(SS),pattern="X")]]; # REMOVE THE COLUMN "X"
#SS[SS[,"StimInterval"] < 0.06 & SS[,"StimInterval"] > 0.04,"StimInterval"] = 0.05; # OPTIMIZE STIMINTERVAL VALUES FOR 0.05
#SS[SS[,"StimInterval"] < 0.11 & SS[,"StimInterval"] > 0.09,"StimInterval"] = 0.1; # OPTIMIZE STIMINTERVAL VALUES FOR 0.1

#================================================= SELECT ROWS ========================================================
#SS = SS[SS[,"Sweep"] ==1,]; # SELECT SS TO CONTAIN ONLY THE FIRST SWEEP

# =========================================== CHANGE COLUMN NAMES  ===================================================
#colnames(EPSC) = paste(colnames(EPSC),".EPSC",sep=""); # CHANGE THE NAMES OF THE COLUMNS TO HAVE A DOT EXTENSION
#colnames(IV) = paste(colnames(IV),".IV",sep=""); # CHANGE THE NAMES OF THE COLUMNS TO HAVE A DOT EXTENSION
#colnames(SS) = paste(colnames(SS),".SS",sep=""); # CHANGE THE NAMES OF THE COLUMNS TO HAVE A DOT EXTENSION

# ================================================== ADD COLUMNS =========================================================
EPSC = cbind(EPSC,Select=1); #  MAKE A NEW COLUMN TO INDICATE SELECTED CELLS
#EPSC$ExpBatch[which(EPSC$ExpDate>=20120509 & EPSC$ExpDate<=20120726)] = 2;
#EPSC$ExpBatch[which(EPSC$ExpDate>=20121003 & EPSC$ExpDate<=20121122)] = 3;
#EPSC$ExpBatch[which(EPSC$ExpDate>=20121003 & EPSC$ExpDate<=20121201)] = 3;
EPSC = ddply(EPSC,c("CellID","Epoch"),function(x){as.data.frame(cbind(x,Count = 1:length(x[,1]))) }); # ADD COUNT COLUMN FOR SWEEP COUNT
# ADD A TIMEINDEX COLUMN: 0 = LAST PRE70 SWEEP, 1 = FIRST PRE40 SWEEP, ONLY TAKE A MAXIMUM OF 20 SWEEPS FROM PRE40
EPSC = ddply(EPSC,c("CellID","Epoch"),function(x){
	if(grepl(unique(x[,"Epoch"]),pat="PRE70")) {first = -(length(x[,1])-1); last=0; firstrow=1; lastrow=length(x[,1]);}
	if(grepl(unique(x[,"Epoch"]),pat="PRE40")) {first=1; last=min(length(x[,1]),20); firstrow=1; lastrow=min(length(x[,1]),20);}
	if(grepl(unique(x[,"Epoch"]),pat="POST70")) {first=21; last=(21+length(x[,1])-1); firstrow=1;lastrow=length(x[,1]);}
	as.data.frame(cbind(x[firstrow:lastrow,],TimeIndex=seq(first,last,by=1))) });
# ADD A NEW COLUM TIME = TIMEINDEX * (25/60)
EPSC = cbind(EPSC,Time=EPSC[,"TimeIndex"]*25/60);
# ADD NEW COLUMN FOR AGE OF THE ANIMAL
EPSC = cbind(EPSC,Age=as.numeric(difftime(strptime(EPSC[,"ExpDate"],"%Y%m%d"),strptime(EPSC[,"BirthDate"],"%Y%m%d"),units="days")) );
# ADD NEW COLUMN FOR AGE OF EXPERIMENTAL DELAY
EPSC = cbind(EPSC,ExpDelay=as.numeric(difftime(strptime(EPSC[,"RecTime"],"%H%M"),strptime(EPSC[,"TreatTime"],"%H%M"),units="mins"))/60 );
# ADD NEW COLUMN FOR MEMBRANE CAPACITANCE
EPSC = cbind(EPSC,MemCap = (1/EPSC$SeriesRes + 1/EPSC$InputRes)*EPSC$TauMem*1E06 ); # UNITS= PICO FARADS
# ADD NEW COLUMN FOR CVs
EPSC = ddply(EPSC,c("CellID","Epoch"),function(x){as.data.frame(cbind(x,CVPeak = (var(x[,"Peak"])-var(x[,"Noise"]) )/(mean(x[,"Peak"])^2),
      CVTail =  (var(x[,"Tail"])-var(x[,"Noise"]) )/(mean(x[,"Tail"])^2)	))});
#-------------------------------------------------------
#SS = ddply(SS,c("CellID","VClamp","StimInterval"),function(x){
#				as.data.frame(cbind(x, NPeak=((x$Peak)/x[,"Peak"][1])*100,TS5Peak = ((x[,"Peak"][5]-x[,"Peak"][1])*100)/x[,"Peak"][1],
#									TS10Peak = ((x[,"Peak"][10]-x[,"Peak"][1])*100)/x[,"Peak"][1], PPFPeak =  ((x[,"Peak"][2])*100)/x[,"Peak"][1],
#									#------------
#									NAreaPeak=((x$Peak)/x[,"AreaPeak"][1])*100,TS5AreaPeak = ((x[,"AreaPeak"][5]-x[,"AreaPeak"][1])*100)/x[,"AreaPeak"][1],
#									TS10AreaPeak = ((x[,"AreaPeak"][10]-x[,"AreaPeak"][1])*100)/x[,"AreaPeak"][1], PPFAreaPeak =  ((x[,"AreaPeak"][2])*100)/x[,"AreaPeak"][1],
#									#------------
#									NTail=((x$Peak)/x[,"Tail"][1])*100,TS5Tail = ((x[,"Tail"][5]-x[,"Tail"][1])*100)/x[,"Tail"][1],
#									TS10Tail = ((x[,"Tail"][10]-x[,"Tail"][1])*100)/x[,"Tail"][1], PPFTail =  ((x[,"Tail"][2])*100)/x[,"Tail"][1],
#									#------------
#									NAreaTail=((x$Peak)/x[,"AreaTail"][1])*100,TS5AreaTail = ((x[,"AreaTail"][5]-x[,"AreaTail"][1])*100)/x[,"AreaTail"][1],
#									TS10AreaTail = ((x[,"AreaTail"][10]-x[,"AreaTail"][1])*100)/x[,"AreaTail"][1], PPFAreaTail =  ((x[,"AreaTail"][2])*100)/x[,"AreaTail"][1],
#									#------------
#									Invivo = strsplit(unique(as.character(x[,"GroupName"])),split="+",fixed=T)[[1]][1],
#									Exvivo = strsplit(unique(as.character(x[,"GroupName"])),split="+",fixed=T)[[1]][2]
#)) });
#=========================================================== SPECIFY THE SELECTION CRITERIA ================================
EPSC$Select[which(grepl(pat="C120120224",EPSC$CellID))] = 0;
weirdIVcells = c("C120121111","C120121130"); #Cells whose IV has a weird shape
shiftedIVcells = c("C220121123","C120120229","C120120613"); # Cells whose reversal potential is shifted too much (< [-20, -30]) to negative side
vclampNeg30highcells = c("C220121106","C220120227");
EPSC[EPSC$CellID%in%c(weirdIVcells,shiftedIVcells,vclampNeg30highcells),"Select"] = 0;
#============================================================ ACTIVATE THE SELECTION =======================================
#EPSC = EPSC[EPSC$Select==1,];
#================================================== ORDER THE LEVELS OF GROUPNAME ========================================
EPSC$GroupName = as.factor(EPSC$GroupName);
EPSC$GroupName = reorder(EPSC$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"));
#------------------------
#SS$GroupName = as.factor(SS$GroupName);
#SS$GroupName = reorder(SS$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"));

#============================================================================================================================
# FIND THE NUMBER OF SWEEPS FOR EACH EPOCH FOR ALL THE CELLS
epsccells = unique(EPSC[,"CellID"]); # GET ALL THE CELLS RECORDED FOR EPSC
nepochs = length(levels(as.factor(EPSC$Epoch)));
epochs = levels(as.factor(EPSC$Epoch));
nsweepsmat = matrix(NA,length(epsccells),nepochs);
i=1;
for (i in 1:length(epsccells))
{
j=1;
for (j in 1:nepochs)
{nsweepsmat[i,j] = length(EPSC[grepl(EPSC$CellID,pat=epsccells[i]) & grepl(EPSC$Epoch,pat=epochs[j]),"Peak"]) };
}
EpscSweep=as.data.frame(nsweepsmat, row.names=NULL);
colnames(EpscSweep)=levels(as.factor(EPSC$Epoch));
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Normalize each cell with it's baseline and add a column with averaged LTP
ltpavgtimestart = 13;
ltpavgtimestop = 16; # Average the last 5 min of Post70 epoch
EPSC = ddply(EPSC,c("CellID"),function(x){
			#------------ AVERAGE VALUE AT PRE70
			peakpre70=mean(x[grepl(x$Epoch,pat="PRE70"),"Peak"]);
			areapeakpre70=mean(x[grepl(x$Epoch,pat="PRE70"),"AreaPeak"]);
			#------------ LENGTHS OF EPOCHS
			npre70 = length(x[grepl(x$Epoch,pat="PRE70"),"Peak"]);
			npre40 = length(x[grepl(x$Epoch,pat="PRE40"),"Peak"]);
			npost70 = length(x[grepl(x$Epoch,pat="POST70"),"Peak"]);
			#------------- normalized values for plotting LTP/potentiation after NMDA activation
			lasttime = max(x[grepl(x$Epoch,pat="POST70"),"Time"]);
			data.frame(cbind(x,
			NPeak = (x[,"Peak"]/peakpre70)*100,
			NAreaPeak = (x[,"AreaPeak"]/areapeakpre70)*100,
			#------------ averaged value: one value/cell and repeated across sweeps
			LTPPeak = mean(x[(grepl(x$Epoch,pat="POST70") & (x$Time >=ltpavgtimestart) & (x$Time <= ltpavgtimestop) ), "Peak"]/peakpre70)*100,
			LTPArea = mean(x[(grepl(x$Epoch,pat="POST70") & (x$Time >=ltpavgtimestart) & (x$Time <= ltpavgtimestop) ), "AreaPeak"]/areapeakpre70)*100,
			#-----------
			AvgPeakPre70 =  mean(x[grepl(x$Epoch,pat="PRE70"),"Peak"]),
			AvgAreaPeakPre70 =  mean(x[grepl(x$Epoch,pat="PRE70"),"AreaPeak"]),
			#----------
			AvgPeakPre40 =  mean(x[grepl(x$Epoch,pat="PRE40"),"Peak"]),
			AvgTailPre40 =  mean(x[grepl(x$Epoch,pat="PRE40"),"Tail"]),
			AvgNPeakPre40 = 	mean(x[grepl(x$Epoch,pat="PRE40"),"Peak"])/abs(peakpre70),
			AvgNTailPre40 = 	mean(x[grepl(x$Epoch,pat="PRE40"),"Tail"])/abs(peakpre70),
			AvgAreaPeakPre40 =  mean(x[grepl(x$Epoch,pat="PRE40"),"AreaPeak"]),
			AvgAreaTailPre40 =  mean(x[grepl(x$Epoch,pat="PRE40"),"AreaTail"]),
			AvgNAreaPeakPre40 =  mean(x[grepl(x$Epoch,pat="PRE40"),"AreaPeak"])/abs(areapeakpre70),
			AvgNAreaTailPre40 =  mean(x[grepl(x$Epoch,pat="PRE40"),"AreaTail"])/abs(areapeakpre70),
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
#==================================================== CELL SELECTION CRITERIA  ==================================================
#EPSC=EPSC[EPSC[,"ExpDate"]<20121123,];  # Select cells that are recorded before 20121123
#SS=SS[SS[,"ExpDate"]<20121123,];  # Select cells that are recorded before 20121123
#IV=IV[IV[,"ExpDate"]<20121123,];  # Select cells that are recorded before 20121123
#======================================================== MERGE DATAFRAME =====================================================
#ELS = merge(EPSCcells,SScells,by=c("CellID","GroupName"),all=T,sort=F);
#EPSC = merge(EPSC,IV[IV$VClamp==0,c("CellID","GroupName","RevPeakCor","RevPeakUncor","RevTailCor","RevTailUncor")],by=c("CellID","GroupName"),all.x=T,sort=F);
#ELS=read.csv("~/DATA/ELS/ELS_CA1_MgFree_NA_Ratio.csv");

#ELSbig = rbind(EPSC,ELS)


