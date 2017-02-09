#ahpo=AHP[AHP$BrainRegion=="HC-CA1" & AHP$Select==1 & AHP$IClamp >=125 & AHP$IClamp <= 450,];
#lineplotmf(ahpo,"FileID","ApNum","GroupName","AvgSpkInt",0,100,c(0,10,20,30,40,50,60,70),"");
#siaaahp=dfAmeliaMI(AHP[AHP$BrainRegion=="HC-CA1" & AHP$Count==1 & AHP$Select==1 & AHP$IClamp >=125 & AHP$IClamp <= 450,],"FileID","AvgSpkInt","IClamp","GroupName",500,"yes",sdtimes=2.7)

#stat = doanovamix(ahp,"FileID","AvgSpkInt","IClamp","GroupName")
#lineplotmf(ahp,"FileID","IClamp","GroupName","AvgSpkInt",0,100,c(0,10,20,30,40,50),"");

#ahp=Famelia(AHP[AHP$BrainRegion=="OFC-LO" & AHP$IClamp==300 & AHP$Select==1 & AHP$Count >=2 & AHP$Count <= 20,],"FileID","SpkInt","Count","GroupName",500,"yes",sdtimes=2.7)
#fig = lineplotmf(ahp,"FileID","Count","GroupName","SpkInt",0,0,c(0,20,40,60,200),"");fig;
#====================The following commands were run to make the final AHP plots on 20131029 ============================
#jpeg(file="/home/goofy/DATA/AHP/R_Plots_Paper/HC_CA1_SpkIntLast.jpg",width=12,height=12,units="cm",bg="white",res=300);
#ahpplot$lineplot;
#dev.off();
#ahp = AHP[AHP$BrainRegion=="PFC-IL" & AHP$Count==1 & AHP$Select==1 & AHP$IClamp >=150 & AHP$IClamp <= 450 & AHP$RSqr >0,]
#ahp=dfAmeliaMI(AHP[AHP$BrainRegion=="HC-CA1" & AHP$Count==1 & AHP$Select==1 & AHP$IClamp >=150 & AHP$IClamp <= 450,],               "FileID","SpkIntLast","IClamp","GroupName",500,"yes",sdtimes=2.7);
#ahpplot=lineplotmf(ahp[[1]],"FileID","IClamp","GroupName","SpkIntLast",25,100,c(25,40,55),c("25","40","55"));
#ggsave(filename="/home/goofy/DATA/AHP/R_Plots_Paper/HC_CA1_SpkIntLast.jpg",plot=ahpplot$lineplot,dpi=300,units="cm",w=12,h=12);
tableparams = c("VRest","InputResf","TauMem","ApAmp","ApRiseTime","ApSlope","ApThres","ApHalfWidth");
table=Fmaketable(AHP[AHP$BrainRegion=="HC-CA1" & AHP$Count==1 & AHP$Select==1 & AHP$IClamp >=125 & AHP$IClamp <= 450,] ,"FileID",tableparams,"IClamp","GroupName","mean");
#====================================================================================================
