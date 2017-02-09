#----------------------- load CSV
rep = read.csv('~/DATA/ELS/CSV/ELS_eEPSC_Rep.csv',header=TRUE,stringsAsFactors=FALSE);
# remove the column "X"
rep=rep[,colnames(rep)[!grepl(colnames(rep),pattern="X")]]  
# remove suprious values by changing them to NA
rep[(rep[,"TauPeak"] > 1 | rep[,"TauPeak"] < 0),"TauPeak"] = NA; 
rep[(rep[,"TauTail"] > 1 | rep[,"TauTail"] < 0),"TauTail"] = NA; 
rep = cbind(rep,"CapMem" = ( ( (1/(rep$InputRes*1e06)) + (1/(rep$SeriesRes*1e06)) ) * (rep$TauMem*1e12 )))
# Add a time axis to the df
rep = cbind(rep,"Time"= seq(from=0,by=(25/60),to=(dim(rep)[1]-1)*(25/60)) );
# remove the averaged sweep
rep = rep[rep[,"Sweep"] > 0,];

# ------------------------------------------The plotting of the multipanel figure
# Make 5 rows in a figure : Peak, Tail, SeriesRes, InputRes, CapMem
#par(mfrow=c(5,1));

#tiff("~/DATA/ELS/ELS_NARep",width=20,height=20,units="cm",compression=c("jpeg"),bg="white",res=300)

m <- c(1,1,1,1,2,2,3,4,5);
layout(m)
# Adjust the inner and outer margins  
par(mar = c(0.7, 2, 0, 0), oma = c(0.7, 1, 1, 1));
# Adjust charater expansion
par(cex = 1.1);
# Adjust the tick length as a fraction of text line height
par(tcl = -0.25)
# Margin line for axis text, title and labels
par(mgp = c(2, 0.6, 0))
# plotting peak
plot(rep$Time,rep$Peak,col="black",frame=F,ann=F,xlim=c(0,45),ylim=c(-500,100))
# Specify the tick marks for xaxis
axis(1, at=seq(0,by=5,to=45), lab=c("0","5","10","15","20","25","30","35","40","45"))
# Draw the zero line
lines(rep$Time,rep(0,dim(rep)[1]),lty=2);
# plotting tail
plot(rep$Time,rep$Tail,col="black",pch=22,frame=F,ann=F,xlim=c(0,45),ylim=c(-10,40),xaxt='n')
#axis(1, at=seq(0,by=5,to=45), lab=c("0","5","10","15","20","25","30","35","40","45"))
# Draw the zero line
lines(rep$Time,rep(0,dim(rep)[1]),lty=2);
# plotting SeriesRes
plot(rep$Time,rep$SeriesRes,col="darkgrey",pch=20,frame=F,ann=F,xlim=c(0,45),xaxt='n')#,ylim=c(15,35))
#axis(1, at=seq(0,by=5,to=45), lab=c("0","5","10","15","20","25","30","35","40","45"));
# Draw the avgerage line
lines(rep$Time,rep(mean(rep$SeriesRes),dim(rep)[1]),lty=2);
#axis(2, at=seq(15,by=20,to=35), lab=c("15","35"));

# plotting InputRes
plot(rep$Time,rep$InputRes,col="darkgrey",pch=20,frame=F,ann=F,xlim=c(0,45),xaxt='n')#,ylim=c(50,150))
#axis(1, at=seq(0,by=5,to=45), lab=c("0","5","10","15","20","25","30","35","40","45"));
# Draw the avgerage line
lines(rep$Time,rep(mean(rep$InputRes),dim(rep)[1]),lty=2);
axis(2, at=seq(50,by=100,to=150), lab=c("50","150"));

# plotting CapMem
plot(rep$Time,rep$CapMem,col="darkgrey",pch=20,frame=F,ann=F,xlim=c(0,45),xaxt='n')#,ylim=c(50,150))
#axis(1, at=seq(0,by=5,to=45), lab=c("0","5","10","15","20","25","30","35","40","45"));
# Draw the avgerage line
lines(rep$Time,rep(mean(rep$CapMem),dim(rep)[1]),lty=2);
#axis(2, at=seq(50,by=100,to=150), lab=c("50","150"));



