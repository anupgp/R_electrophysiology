
#----------------------- ss70100
ss70100 = read.csv('~/DATA/ELS/CSV/ELS_ss70100.csv',header=TRUE,stringsAsFactors=FALSE);
# remove the column "X"
ss70100 = ss70100[,colnames(ss70100)[!grepl(colnames(ss70100),pattern="X")]];
# only keep the averaged sweep
#ss70100 = ss70100[ss70100[,"Sweep"] == min(ss70100$Sweep),];
ss70100 = ss70100[ss70100[,"Sweep"] == 1,]; # Take only the first trace
# Normalize the EPSCs w.r.t. the first EPSC peak amplitude
nss70100pamp = ddply(ss70100,.(CellID),function(x){data.frame(Peak=((x$Peak)/x[,"Peak"][1])*100,GroupName=unique(x$GroupName),StimNum=unique(x$StimNum),TS5 = ((x[,"Peak"][5]-x[,"Peak"][1])*100)/x[,"Peak"][1],
TS10 = ((x[,"Peak"][10]-x[,"Peak"][1])*100)/x[,"Peak"][1],PPF =  ((x[,"Peak"][2])*100)/x[,"Peak"][1]   )});
nss70100parea = ddply(ss70100,.(CellID),function(x){data.frame(AreaPeak=(x$AreaPeak*100)/x[,"AreaPeak"][1],GroupName=unique(x$GroupName),StimNum=unique(x$StimNum))});
# Order Factors for nss70100pamp
nss70100pamp$GroupName = as.factor(nss70100pamp$GroupName)
nss70100pamp$GroupName = reorder(nss70100pamp$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Create the additional factors for two factor ANOVA
factors=str_split_fixed(nss70100pamp$GroupName,"\\+",2)
nss70100pamp = cbind(nss70100pamp,"Treat_invivo"=as.character(factors[,1]),"Treat_exvivo"=as.character(factors[,2]))

#----------------------- ss7050
ss7050 = read.csv('~/DATA/ELS/CSV/ELS_ss7050.csv',header=TRUE,stringsAsFactors=FALSE);
# remove the column "X"
ss7050 = ss7050[,colnames(ss7050)[!grepl(colnames(ss7050),pattern="X")]];
# only keep the averaged sweep
ss7050 = ss7050[ss7050[,"Sweep"] == 1,];
# Normalize the EPSCs w.r.t. the first EPSC peak amplitude
nss7050pamp = ddply(ss7050,.(CellID),function(x){data.frame(Peak=((x$Peak)/x[,"Peak"][1])*100,GroupName=unique(x$GroupName),StimNum=unique(x$StimNum),TS5 = ((x[,"Peak"][5]-x[,"Peak"][1])*100)/x[,"Peak"][1],
TS10 = ((x[,"Peak"][10]-x[,"Peak"][1])*100)/x[,"Peak"][1],PPF =  ((x[,"Peak"][2])*100)/x[,"Peak"][1]   )});
nss7050parea = ddply(ss7050,.(CellID),function(x){data.frame(AreaPeak=(x$AreaPeak*100)/x[,"AreaPeak"][1],GroupName=unique(x$GroupName),StimNum=unique(x$StimNum))});
# Order Factors for nss7050amp
nss7050pamp$GroupName = as.factor(nss7050pamp$GroupName)
nss7050pamp$GroupName = reorder(nss7050pamp$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Create the additional factors for two factor ANOVA
factors=str_split_fixed(nss7050pamp$GroupName,"\\+",2)
nss7050pamp = cbind(nss7050pamp,"Treat_invivo"=as.character(factors[,1]),"Treat_exvivo"=as.character(factors[,2]))


#----------------------- ss40100
ss40100 = read.csv('~/DATA/ELS/CSV/ELS_ss40100.csv',header=TRUE,stringsAsFactors=FALSE);
# remove the column "X"
ss40100 = ss40100[,colnames(ss40100)[!grepl(colnames(ss40100),pattern="X")]];
# only keep the averaged sweep
ss40100 = ss40100[ss40100[,"Sweep"] < 0,];
# Peak Amp: Normalize the EPSCs w.r.t. the first EPSC peak amplitude
nss40100pamp = ddply(ss40100,.(CellID),function(x){data.frame(Peak=((x$Peak)/x[,"Peak"][1])*100,GroupName=unique(x$GroupName),StimNum=unique(x$StimNum),TS5 = ((x[,"Peak"][5]-x[,"Peak"][1])*100)/x[,"Peak"][1],
TS10 = ((x[,"Peak"][10]-x[,"Peak"][1])*100)/x[,"Peak"][1],PPF =  ((x[,"Peak"][2])*100)/x[,"Peak"][1]   )});
# Tail Amp: 40100
nss40100tamp = ddply(ss40100,.(CellID),function(x){data.frame(Tail=((x$Tail)/x[,"Tail"][1])*100,GroupName=unique(x$GroupName),StimNum=unique(x$StimNum),TS5 = ((x[,"Tail"][5]-x[,"Tail"][1])*100)/x[,"Tail"][1],
TS10 = ((x[,"Tail"][10]-x[,"Tail"][1])*100)/x[,"Tail"][1],PPF =  ((x[,"Tail"][2])*100)/x[,"Tail"][1]   )});
#Peak Area 40100
nss40100parea = ddply(ss40100,.(CellID),function(x){data.frame(AreaPeak=((x$AreaPeak)/x[,"AreaPeak"][1])*100,GroupName=unique(x$GroupName),StimNum=unique(x$StimNum),
TS5 = ((x[,"AreaPeak"][5]-x[,"AreaPeak"][1])*100)/x[,"AreaPeak"][1], TS10 = ((x[,"AreaPeak"][10]-x[,"AreaPeak"][1])*100)/x[,"AreaPeak"][1],PPF =  ((x[,"AreaPeak"][2])*100)/x[,"AreaPeak"][1]   )});
#Tail Area 40100
nss40100tarea = ddply(ss40100,.(CellID),function(x){data.frame(AreaTail=((x$AreaTail)/x[,"AreaTail"][1])*100,GroupName=unique(x$GroupName),StimNum=unique(x$StimNum),
TS5 = ((x[,"AreaTail"][5]-x[,"AreaTail"][1])*100)/x[,"AreaTail"][1], TS10 = ((x[,"AreaTail"][10]-x[,"AreaTail"][1])*100)/x[,"AreaTail"][1],PPF =  ((x[,"AreaTail"][2])*100)/x[,"AreaTail"][1]   )});

# Order Factors for nss40100pamp & nss40100parea
nss40100pamp$GroupName = as.factor(nss40100pamp$GroupName)
nss40100pamp$GroupName = reorder(nss40100pamp$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
nss40100parea$GroupName = as.factor(nss40100parea$GroupName)
nss40100parea$GroupName = reorder(nss40100parea$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
nss40100tamp$GroupName = as.factor(nss40100tamp$GroupName)
nss40100tamp$GroupName = reorder(nss40100tamp$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
nss40100tarea$GroupName = as.factor(nss40100tarea$GroupName)
nss40100tarea$GroupName = reorder(nss40100tarea$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Create the additional factors for two factor ANOVA
factors=str_split_fixed(nss40100pamp$GroupName,"\\+",2)
nss40100pamp = cbind(nss40100pamp,"Treat_invivo"=as.character(factors[,1]),"Treat_exvivo"=as.character(factors[,2]));
factors=str_split_fixed(nss40100parea$GroupName,"\\+",2)
nss40100parea = cbind(nss40100parea,"Treat_invivo"=as.character(factors[,1]),"Treat_exvivo"=as.character(factors[,2]))
factors=str_split_fixed(nss40100tamp$GroupName,"\\+",2)
nss40100tamp = cbind(nss40100tamp,"Treat_invivo"=as.character(factors[,1]),"Treat_exvivo"=as.character(factors[,2]));
factors=str_split_fixed(nss40100tarea$GroupName,"\\+",2)
nss40100tarea = cbind(nss40100tarea,"Treat_invivo"=as.character(factors[,1]),"Treat_exvivo"=as.character(factors[,2]))


#----------------------- ss4050
ss4050 = read.csv('~/DATA/ELS/CSV/ELS_ss4050.csv',header=TRUE,stringsAsFactors=FALSE);
# remove the column "X"
ss4050 = ss4050[,colnames(ss4050)[!grepl(colnames(ss4050),pattern="X")]];
# only keep the averaged sweep
ss4050 = ss4050[ss4050[,"Sweep"] < 0,];
# Peak Amp: Normalize the EPSCs w.r.t. the first EPSC peak amplitude
nss4050pamp = ddply(ss4050,.(CellID),function(x){data.frame(Peak=((x$Peak)/x[,"Peak"][1])*100,GroupName=unique(x$GroupName),StimNum=unique(x$StimNum),TS5 = ((x[,"Peak"][5]-x[,"Peak"][1])*100)/x[,"Peak"][1],
TS10 = ((x[,"Peak"][10]-x[,"Peak"][1])*100)/x[,"Peak"][1],PPF =  ((x[,"Peak"][2])*100)/x[,"Peak"][1]   )});
# Tail Amp: 4050
nss4050tamp = ddply(ss4050,.(CellID),function(x){data.frame(Tail=((x$Tail)/x[,"Tail"][1])*100,GroupName=unique(x$GroupName),StimNum=unique(x$StimNum),TS5 = ((x[,"Tail"][5]-x[,"Tail"][1])*100)/x[,"Tail"][1],
TS10 = ((x[,"Tail"][10]-x[,"Tail"][1])*100)/x[,"Tail"][1],PPF =  ((x[,"Tail"][2])*100)/x[,"Tail"][1]   )});
#Peak Area 4050
nss4050parea = ddply(ss4050,.(CellID),function(x){data.frame(AreaPeak=((x$AreaPeak)/x[,"AreaPeak"][1])*100,GroupName=unique(x$GroupName),StimNum=unique(x$StimNum),
TS5 = ((x[,"AreaPeak"][5]-x[,"AreaPeak"][1])*100)/x[,"AreaPeak"][1], TS10 = ((x[,"AreaPeak"][10]-x[,"AreaPeak"][1])*100)/x[,"AreaPeak"][1],PPF =  ((x[,"AreaPeak"][2])*100)/x[,"AreaPeak"][1]   )});
#Tail Area 4050
nss4050tarea = ddply(ss4050,.(CellID),function(x){data.frame(AreaTail=((x$AreaTail)/x[,"AreaTail"][1])*100,GroupName=unique(x$GroupName),StimNum=unique(x$StimNum),
TS5 = ((x[,"AreaTail"][5]-x[,"AreaTail"][1])*100)/x[,"AreaTail"][1], TS10 = ((x[,"AreaTail"][10]-x[,"AreaTail"][1])*100)/x[,"AreaTail"][1],PPF =  ((x[,"AreaTail"][2])*100)/x[,"AreaTail"][1]   )});

# Order Factors for nss4050pamp & nss4050parea
nss4050pamp$GroupName = as.factor(nss4050pamp$GroupName)
nss4050pamp$GroupName = reorder(nss4050pamp$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
nss4050parea$GroupName = as.factor(nss4050parea$GroupName)
nss4050parea$GroupName = reorder(nss4050parea$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
nss4050tamp$GroupName = as.factor(nss4050tamp$GroupName)
nss4050tamp$GroupName = reorder(nss4050tamp$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
nss4050tarea$GroupName = as.factor(nss4050tarea$GroupName)
nss4050tarea$GroupName = reorder(nss4050tarea$GroupName, new.order = c("CTRL+VEH","CTRL+CORT","ELS+VEH","ELS+CORT"))
# Create the additional factors for two factor ANOVA
factors=str_split_fixed(nss4050pamp$GroupName,"\\+",2)
nss4050pamp = cbind(nss4050pamp,"Treat_invivo"=as.character(factors[,1]),"Treat_exvivo"=as.character(factors[,2]));
factors=str_split_fixed(nss4050parea$GroupName,"\\+",2)
nss4050parea = cbind(nss4050parea,"Treat_invivo"=as.character(factors[,1]),"Treat_exvivo"=as.character(factors[,2]))
factors=str_split_fixed(nss4050tamp$GroupName,"\\+",2)
nss4050tamp = cbind(nss4050tamp,"Treat_invivo"=as.character(factors[,1]),"Treat_exvivo"=as.character(factors[,2]));
factors=str_split_fixed(nss4050tarea$GroupName,"\\+",2)
nss4050tarea = cbind(nss4050tarea,"Treat_invivo"=as.character(factors[,1]),"Treat_exvivo"=as.character(factors[,2]))















