source("/mnt/storage/goofy/projects/codes/els/R/loadLibraries.R");
options(contrasts=c("contr.sum","contr.poly"));
options("width"= 170);
## Loading full morphology data
fcall = read.csv('/mnt/storage/goofy/projects/data/els/cooked/els_fc.csv',sep=",",header=TRUE,stringsAsFactors=FALSE,row.names=NULL);
## levels of mode: train, test
## levels of type: before, after, during
## levels of group: els, control
## levels of exp: context, cue
## ---------------------------------------------------
## stats and plots for training contextual conditioning
## ---------------------------------------------------
fc=fcall[fcall[,"exp"]=="context" & fcall[,"mode"]=="test",]; 
## fc=fcall[fcall[,"exp"]=="cue" & fcall[,"mode"]=="test" & fcall[,"type"] == "after",];
## fc=fcall[fcall[,"exp"] == "context" & fcall[,"mode"] == "train" & fcall[,"type"] == "before",];
## fc=fcall[fcall[,"exp"] == "context" & fcall[,"mode"] == "train" & fcall[,"type"] == "after",];
## fc=fcall[fcall[,"exp"] == "context" & fcall[,"mode"] == "train",];
fc=fcall[fcall[,"exp"] == "cue" & fcall[,"mode"] == "test",];
## make factor and order levels
fc=cbind(fc,Group=fc$group); ## group as factor
fc=cbind(fc,Type=fc$type); ## type as factor
fc$Group = factor(fc$Group,levels=c("control","els"),ordered=TRUE);
fc$Type = factor(fc$Type,levels=c("before","after"),ordered=TRUE);
bargraph.CI(x.factor = type,response = freezing, group = group,data=fc);
# Processing freezing
aggregate(freezing ~ group, FUN=function(x){sum(!is.na(x))},data=fc);
aggregate(freezing ~ group, FUN=function(x){mean(x,na.rm = TRUE)},data=fc);
aggregate(freezing ~ group, FUN=function(x){se(x,na.rm = TRUE)},data=fc);
lmstat2f=lm(freezing ~ (group * type),data=fc);summary(lmstat2f);
aovstat=aov(freezing ~ group * type + Error(subject),data=fc);summary(aovstat);
t.test(freezing ~ group, alternative = "two.sided", paired = FALSE, var.equal = FALSE, data = fc);
## Check homogeneity of variance | check for equal variance
bartlett.test(freezing ~ Group,data = fc[fc[,"type"] == "after",]);
# Check normality on data of each group
by(fc[fc[,"type"] == "after","freezing"],fc[fc[,"type"] == "after","group"],shapiro.test);
## t-test
t.test(freezing ~ Group,alternative = "two.sided",paired = FALSE, var.equal = FALSE, data = fc[fc[,"type"] == "after",]);
## average across groups: contextual training freezing
fccontrainavg=ddply(fc[fc[,"type"] == "after",],c("group"),function(x){dftemp=data.frame("group"=unique(x$group),"y.avg"=mean(x$freezing,na.rm=TRUE),"y.sem"=se(x$freezing,na.rm=TRUE));return(dftemp);});
jpeg("/mnt/storage/goofy/projects/data/els/figures/fccontraining.jpeg",width=12,height=18,units="cm",bg="transparent",res=300)
par(mar=c(3,4,2.2,3));par(mgp=c(4,1,0));par(lwd = 3); #(bottom, left,top,right)
bargraph.CI(x.factor = Type, response = freezing, group = Group, data = fc, split=FALSE, cex.names = 0.65, cex.lab = 2, xlim = c(0.5,6), ylim = c(0,100), cex.axis = 3, cex.main = 2, lwd = 3, err.lty = 1, err.col = "black", font.axis=1, family="sans", font=1, font.lab=1,density = c(0,100), angle = c(0,-45,45,45), legend = F, x.leg = 0, y.leg = 0.5,cex.leg = 2, ylab=" Duration ",col = c("black","black"));
## ---------------------
jpeg("/mnt/storage/goofy/projects/data/els/figures/fccuetestafter.jpeg",width=10,height=18,units="cm",bg="transparent",res=300)
fc=fcall[fcall[,"exp"]=="cue" & fcall[,"mode"]=="test",];
## make factor and order levels
fc=cbind(fc,Group=fc$group); ## group as factor
fc=cbind(fc,Type=fc$type); ## type as factor
fc$Group = factor(fc$Group,levels=c("control","els"),ordered=TRUE);
fc$Type = factor(fc$Type,levels=c("before","during","after"),ordered=TRUE);
## average across groups: contextual training freezing
fccontrainavg=ddply(fc[fc[,"type"] == "after",],c("group"),function(x){dftemp=data.frame("group"=unique(x$group),"y.avg"=mean(x$freezing,na.rm=TRUE),"y.sem"=se(x$freezing,na.rm=TRUE));return(dftemp);});
par(bg="white",fg="black",mar=c(5,5,4,2)+c(0,3.5,0,0),lwd=4);
plotavg=c(fccontrainavg[fccontrainavg$group=="control","y.avg"],fccontrainavg[fccontrainavg$group=="els","y.avg"]);
plotsem=c(fccontrainavg[fccontrainavg$group=="control","y.sem"],fccontrainavg[fccontrainavg$group=="els","y.sem"]);
## plot average 
barplot(plotavg,col=c("white","black"),xlim=c(0,2.5),ylim=c(0,100),axes=FALSE,fg="black");
## plot error bars
arrows(x0=0.7, y0=plotavg[1]-plotsem[1], x1=0.7, y1=plotavg[1]+ plotsem[1],col="black",lwd=4,length=0.05,angle=90,code=3);
arrows(x0=1.9, y0=plotavg[2], x1=1.9, y1=plotavg[2]+plotsem[2],col="black",lwd=4,length=0.05,angle=90,code=3);
arrows(x0=1.9, y0=plotavg[2], x1=1.9, y1=plotavg[2] - plotsem[2],col="white",lwd=3,length=0.05,angle=90,code=3,bg="white");
axis(2,at=c(0,25,50,75,100),labels=as.character(c("0","25","50","75","100")),tick=T,lwd=3,cex.axis=3.5,xpd=F,las=1,padj=0.5);
dev.off();
## ---------------------
abline(h = 1, col = "grey", lwd = 0.1, lty = 2);
## Mixed design ANOVA having both within and between subject factors. Ref: http://www.cookbook-r.com/Statistical_analysis/ANOVA/
fcaov = aov(freezing ~ (group*type) + Error(subject/type), data = fc);summary(fcaov);
