source("/mnt/storage/goofy/projects/codes/els/R/loadLibraries.R");
options(contrasts=c("contr.sum","contr.poly"));
options("width"= 170);
orm = read.csv('/mnt/storage/goofy/projects/data/els/cooked/els_orm_data.csv',sep=",",header=TRUE,stringsAsFactors=FALSE,row.names=NULL);
#---------------------- 07 July 2015 Statistics on the Object recognition memory data send by Harm
# Processing Frequency
aggregate(frequency ~ group, FUN=function(x){sum(!is.na(x))},data=orm);
aggregate(frequency ~ group, FUN=function(x){mean(x,na.rm = TRUE)},data=orm);
aggregate(frequency ~ group, FUN=function(x){se(x,na.rm = TRUE)},data=orm);
aovstat=aov(frequency ~ group,data=orm);summary(aovstat);
kruskal.test(frequency ~ group,data=orm);
lmstat1f=lm(frequency ~ groups,data=orm);summary(lmstat1f);
lmstat2f=lm(frequency ~ (earlytreat * latetreat),data=orm);summary(lmstat2f);
lmstat2f=lm(duration ~ (earlytreat * latetreat),data=orm);summary(lmstat2f);
aovstat=aov(frequency ~ earlytreat * latetreat + Error(subject),data=orm);summary(aovstat);
## -----Check for the assumptions of ANOVA: Homogeneity of variance & normality
# Check normality on model residual
shapiro.test(residuals(lmstat2f));
# Check normality on data of each group
by(orm$frequency,orm$group,shapiro.test);
# A very standard test for homogenity of variance
leveneTest(lmstat2f);
#Check homogeneity of variance
bartlett.test(frequency ~ group,data=orm);
## plotting
boxplot(frequency ~ groups,data=orm);
lineplot.CI(x.factor = group, response = frequency, group = groups, fixed = T, data = orm); 

## Stats ORM data
aggregate(frequency ~ groups, FUN=function(x){sum(!is.na(x))},data=datum);
lmstat2f = lm(frequency ~ earlytreat*latetreat,data=orm);summary(lmstat2f);
lmstat2f = lm(duration ~ earlytreat*latetreat,data=orm);summary(lmstat2f);
## pairwise tests
pairwise.t.test(orm$frequency, orm$group,p.adjust.method = "bonf",paired=FALSE,alternative = "two.sided");
pairwise.t.test(orm$duration, orm$groups,p.adjust.method = "bonf",paired=FALSE,alternative = "two.sided");
pairwise.wilcox.test(x=orm[,"frequency"], g=orm[,"group"],p.adjust = "bonf",paired=FALSE,alternative = "two.sided");
pairwise.wilcox.test(x=orm[,"duration"], g=orm[,"group"],p.adjust = "bonf",paired=FALSE,alternative = "two.sided");
## t-test
t.test(duration ~ group,alternative = "two.sided",paired = FALSE, var.equal = FALSE, data = orm[orm[,"latetreat"]=="norm",]);
t.test(frequency ~ group,alternative = "two.sided",paired = FALSE, var.equal = FALSE, data = orm[orm[,"latetreat"]=="stress",]);

## --------------------------------------------------
## jpeg("/mnt/storage/goofy/projects/data/els/figures/ormvisits2.jpeg",width=10,height=18,units="cm",bg="transparent",res=300)
jpeg("/mnt/storage/goofy/projects/data/els/figures/ormduration_stress.jpeg",width=10,height=20,units="cm",bg="transparent",res=300)
## par(bg="white",fg="black",mar=c(5,5,4,2)+c(0,3.5,0,0),lwd=4);
par(mar=c(3,6,2.2,3));par(mgp=c(4,1,0));par(lwd = 3); #(bottom, left,top,right)
par(bg="white");
ormt=orm;
## make factor and order levels
ormt=cbind(ormt,Group=ormt$group); ## group as factor
ormt$Group = factor(ormt$Group,levels=c("norm-ctrl","norm-els","stress-ctrl","stress-els"),ordered=TRUE); # order group
ormt=cbind(ormt,EarlyTreat=ormt$earlytreat); ## earlytreat as factor
ormt$EarlyTreat = factor(ormt$EarlyTreat,levels=c("ctrl","els"),ordered=TRUE); # order earlytreat
ormt=cbind(ormt,LateTreat=ormt$latetreat); ## latetreat as factor
ormt$LateTreat = factor(ormt$LateTreat,levels=c("norm","stress"),ordered=TRUE); # order latetreat
## average across groups: contextual training freezing
ormtavg=ddply(ormt,c("group"),function(x){dftemp=data.frame("group"=unique(x$group),"freq.avg"=mean(x$frequency,na.rm=TRUE)*100,"freq.sem"=se(x$frequency,na.rm=TRUE)*100,"dur.avg"=mean(x$duration,na.rm=TRUE)*100,"dur.sem"=se(x$duration,na.rm=TRUE)*100);return(dftemp);});
plotavg=c(ormtavg[ormtavg$group=="stress-ctrl","dur.avg"],ormtavg[ormtavg$group=="stress-els","dur.avg"]);
plotsem=c(ormtavg[ormtavg$group=="stress-ctrl","dur.sem"],ormtavg[ormtavg$group=="stress-els","dur.sem"]);
## plot average
## barplot(plotavg,col=c("white","grey"),xlim=c(0,2.5),ylim=c(0,100),axes=FALSE); # first call to fill the bars
barplot(plotavg,col=c("white","black"),xlim=c(0,2.5),ylim=c(0,100),axes=FALSE); # first call to fill the bars
barplot(plotavg,col=c("black"),xlim=c(0,2.5),ylim=c(0,100),axes=FALSE,density=c(10,0),angle=c(45,0),fg=c("black"),border=c("black"),add=TRUE); # second call to shade
## plot error bars
arrows(x0=0.7, y0=plotavg[1]-plotsem[1], x1=0.7, y1=plotavg[1]+ plotsem[1],col="black",lwd=4,length=0.05,angle=90,code=3);
## arrows(x0=1.9, y0=plotavg[2]-plotsem[2], x1=1.9, y1=plotavg[2]+ plotsem[2],col="black",lwd=4,length=0.05,angle=90,code=3);
arrows(x0=1.9, y0=plotavg[2], x1=1.9, y1=plotavg[2]+plotsem[2],col="black",lwd=4,length=0.05,angle=90,code=3);
arrows(x0=1.9, y0=plotavg[2], x1=1.9, y1=plotavg[2] - plotsem[2],col="white",lwd=3,length=0.05,angle=90,code=3,bg="white");
axis(2,at=c(0,25,50,75,100),labels=as.character(c("0","25","50","75","100")),tick=T,lwd=3,cex.axis=2.5,xpd=F,las=1,padj=0.5);
dev.off();
## ------------------------------------------------
## Plot ORM data %Duration
par(mar=c(4,6.5,5,5));par(mgp=c(4,1,0));par(lwd = 2);
bargraph.CI(x.factor = group, response = duration*100, group = LateTreat, split = FALSE, fixed = T, data = orm, space = 0.5,cex.names = 0.65, cex.lab = 2, ylim = c(0,80), main = "ORM", cex.axis = 3, cex.main = 2, lwd = 2, err.lty = 1, err.col = "black", font.axis=1, family="sans", font=1, font.lab=1,density = c(0,100,10,100), angle = c(0,-45,45,45), col = c("black","grey","black","black"), legend = F, x.leg = 0, y.leg = 0.5,cex.leg = 2, ylab=" Duration ");
abline(h = 1, col = "grey", lwd = 0.1, lty = 2);

## Plot ORM data %Frequency
par(mar=c(4,6.5,5,5));par(mgp=c(4,1,0));par(lwd = 2);
bargraph.CI(x.factor = group, response = frequency*100, group = group, fixed = T, data = orm, space = 0.1,cex.names = 0.65, cex.lab = 2, ylim = c(0,80), main = "ORM", cex.axis = 3, cex.main = 2, lwd = 2, err.lty = 1, err.col = "black", font.axis=1, family="sans", font=1, font.lab=1,density = c(0,100,10,100), angle = c(0,-45,45,45), col = c("black","grey","black","black"), legend = F, x.leg = 0, y.leg = 0.5,cex.leg = 2, ylab=" Duration ");
abline(h = 1, col = "grey", lwd = 0.1, lty = 2);
