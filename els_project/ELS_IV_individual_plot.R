lineplot.CI(VClampA,Peak,GroupName,data=EPSC[EPSC$RecMode=="EPSCIV",]);
abline(v=10,col="red");abline(h=0,col="red");
lineplot.CI(VClampA,Tail,GroupName,data=EPSC[EPSC$RecMode=="EPSCIV",]);
t(save = "no")
abline(v=10,col="red");abline(h=0,col="red");
parOB(mfrow=c(2,1));
bargraph.CI(Groups,RevPeakCor,Groups,data=EPSC[EPSC$RecMode=="EPSCIV" & EPSC$Count==1,]);
t(save = "no")
qargraph.CI(Groups,RevTailCor,Groups,data=EPSC[EPSC$RecMode=="EPSCIV" & EPSC$Count==1,]);
