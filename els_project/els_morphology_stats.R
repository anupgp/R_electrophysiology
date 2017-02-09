## Statistics
## Clear all
rm(list=ls());
options(contrasts=c("contr.sum","contr.poly"));
source("/mnt/storage/goofy/projects/codes/els/R/els_morphology_preprocessing.R");
## Create seperate dataframe for area(ca1/bla) and sides (apical/basal/full)
morca1a=morall[morall[,"area"]=="ca1" & morall[,"side"]=="apical",];
morca1b=morall[morall[,"area"]=="ca1" & morall[,"side"]=="basal" ,];
morbla=morall[morall[,"area"]=="bla" & morall[,"side"]=="full" & morall[,"radius"] <= 10000,];
morbla=morall[morall[,"area"]=="bla" & morall[,"side"]=="full" & morall[,"radius"] >=100,];
## Load the right dataframe for statistics
mor = morbla;
# Averages and number of samples per group
aggregate(dl ~ group, FUN=function(x){sum(!is.na(x))},data=mor,subset = mor$radius==10);
## artificailly change data in one group to check the two-way ANOVA 
## mor[mor[,"group"]=="els","dl"] = mor[morbla[,"group"]=="els","dl"] + (0.2 * mor[morbla[,"group"]=="els","dl"]); 
## Make a new Subject, within Groups and between group variables that are factorized
mor=cbind(mor,Cellid=mor$cellid); ## cell identifier
mor$Cellid = as.factor(mor$Cellid);
## ------
mor=cbind(mor,Radius=mor$radius); ## Sholl radii
mor$Radius = as.factor(mor$Radius);
## -----
mor=cbind(mor,Group=mor$group); ## Experimental groups
mor$Group = factor(mor$Group,levels=c("control","els"),ordered=TRUE);
## ----
## Mixed design ANOVA having both within and between subject factors. Ref: http://www.cookbook-r.com/Statistical_analysis/ANOVA/
moraov = aov(dl ~ (Group*Radius) + Error(Cellid/Radius), data = mor);summary(moraov);
## print(model.tables(moraov,"means"),digits=3);
jpeg("/mnt/storage/goofy/projects/data/els/figures/morph_sholl_ca1_basal_box.jpeg",width=30,height=15,units="cm",bg="transparent",res=300)
par(bg="white",fg="black",mar=c(5,4,4,2)+c(0,3.5,0,0),lwd=1);
Boxplot(dl~Group*radius,data=mor[mor$radius<1000,], col=(c("white","black")),notch=TRUE,labels=cellid,xlab="Sholl distance",cex=0.5)
legend("topright",legend=c("Control","ELS"),fill=c("white","black"),cex=0.8,horiz=FALSE,border="black");
dev.off();
## other stats tried: successfull!
my.control=lmerControl(optCtrl=list(maxfun=20000));
mf=lme(dl ~ (Radius * Group), random = ~1|Cellid/Radius,data=mor);summary(mf);
anova(mf);
## Allow cells/subjects to have different slopes
mf2=lmer(dl ~ (Radius * Group) + (1|Cellid),mor);anova(mf2);fixef(mf2);ranef(mf2);
## Allow groups to have different slopes
mf3=lmer(dl ~ (Radius * Group) + (Group|Cellid),mor);anova(mf3);fixef(mf3);ranef(mf3);
## test for sphericity
## round off decimal places
mor2=mor;mor2$dl=round(mor2$dl, digits = 2)
## make a wide format data for morall
morw=reshape(mor2[,c("cellid","group","radius","dl")],direction="wide",v.names="dl",idvar=c("cellid"),timevar=c("radius"))
## multivariate approach
## Reorganize the data into matrix form where rows are subjects columns are levels of the repeated measures factor
mormat = with(morw[morw[,"group"]=="control",],cbind(morw[morw[,"group"]=="control",colnames(morw)[grepl("dl",colnames(morw))]]));mormat=as.matrix(mormat);
## mormat = with(morw,cbind(morw[,colnames(morw)[grepl("dl",colnames(morw))]]));mormat=as.matrix(mormat);
mormat2= matrix(mormat,dim(mormat));
morfact = expand.grid(radius=gl(20, 1))
## mormat2= mormat[,1:15];
## mormat2[mormat2==0]=NA;
mvlm = lm(mormat2 ~ 1);
## define factor for radius
shollradius = factor(seq(10,150,10));
shollradius = factor(sort(unique(mor$radius)),ordered = TRUE);
## define anova model using the Anova() function in car package
mvlm.aov = Anova(mvlm, idata = morfact, idesign = ~radius, type = "III");summary(mvlm.aov,multivariate = FALSE);
anova(mvlm, M=~radius, X=~1, idata=morfact, test="Spherical")

## print(mf,correlation=TRUE);
mauchly.test(mf);
anova(mf)
## Check homogeneity of variance
plot(mf);
## Check normality
qqnorm(mf,~ranef(.,level=2));
## ==================================================
## statistics on total dendritic length
## Create seperate dataframe for area(ca1/bla) and sides (apical/basal/full)
mortca1a=morallt[morallt[,"area"]=="ca1" & morallt[,"side"]=="apical",];
mortca1b=morallt[morallt[,"area"]=="ca1" & morallt[,"side"]=="basal" ,];
mortbla=morallt[morallt[,"area"]=="bla" & morallt[,"side"]=="full",];
## Load the right dataframe for statistics
mort = mortbla;
mort=cbind(mort,Cellid=mort$cellid); ## cell identifier
mort$Cellid = as.factor(mort$Cellid);
## ------
mort=cbind(mort,Group=mort$group); ## Experimental groups
mort$Group = factor(mort$Group,levels=c("control","els"),ordered=TRUE);
## ----
## T-test R
mort.t = t.test(dl ~ (Group), data = mort,paired=FALSE,alternative = "two.sided");mort.t;
## Normality test
shapiro.test(mort[mort[,"group"]=="control","dl"]);
qqnorm(mort[mort[,"group"]=="control","dl"]);
qqline(mort[mort[,"group"]=="control","dl"]);
## --------------
## jpeg("/mnt/storage/goofy/projects/data/els/figures/morph_total_ca1_basal_box.jpeg",width=18,height=15,units="cm",bg="transparent",res=300)
## par(bg="white",fg="black",mar=c(5,4,4,2)+c(0,3.5,0,0),lwd=1);
## Boxplot(dl~Group,data=mort, col=(c("white","black")),notch=TRUE,labels=cellid,xlab="Groups",cex=0.5)
## legend("topright",legend=c("Control","ELS"),fill=c("white","black"),cex=0.8,horiz=FALSE,border="black");
## dev.off();
## average plot
mortavg=ddply(mort,c("group"),function(x){
    grp=unique(x$group);
    dl.avg=mean(x$dl,na.rm=TRUE);
    dl.sem=se(x$dl,na.rm=TRUE);
    df_temp=data.frame("group"=grp,"dl.avg"=dl.avg,"dl.sem"=dl.sem);
    return(df_temp);});
## average plot
bargraph.CI(x.factor = group, response = dl, group = group, data = mort,ylim=c(0,2500));

## old codes for morphology
## Exploratory ploting on morphology (CA1 & BLA) 
lineplot.CI(x.factor = radius, response = dl, group = group,fun=function(x){mean(x,na.rm=TRUE)},data = morall[morall$area=="ca1" & morall$side=="apical",]);
lineplot.CI(x.factor = radius, response = dl, group = group,data = morall[morall$area=="bla" & morall$side=="full",],ylim=c(0,50));
lineplot.CI(x.factor = radius, response = dl, data = morall[morall$area=="bla" & morall$group=="els",]);
bargraph.CI(x.factor = group, response = dl, group = group, data = morallt[morallt$area=="bla" & morallt$side=="full",],ylim=c(0,250));
qplot(x=radius,y=dl,data=morall[morall$area=="bla" & morall$side=="full",],color=group,geom="boxplot")
Boxplot(dl ~ group * radius, data=morall[morall$area=="ca1" & morall$side=="apical" & (morall$group=="control" | morall$group=="els") & morall$Radius <= 1000,],labels=cellid)
## Statistics
my.control=lmerControl(optCtrl=list(maxfun=20000));
mf=lmer(dl ~ radius * group + (1+radius|cellid),morall[morall$Area=="ca1" & morall$Side=="apical",],control=my.control); 
print(mf,correlation=TRUE);
summary(mf);
anova(mf);
## Sholl plot with error bars
mor_sub = morall[morall$side=="total" & morall$area == "bla",];
## jpeg("/mnt/storage/goofy/projects/data/els/figures/bla_total_sholl.jpeg",width=22,height=18,units="cm",bg="transparent",res=300);
par(bg="white",fg="black");par(omi = c(0.3,0.3,0.3,0.3));
xscale=seq(0,200,50);xscalelab=as.character(xscale);
yscale=seq(0,25,5);yscalelab=as.character(yscale);
plot(x=mor_sub[mor_sub$group=="ctrl",]$sholl_radius,y=mor_sub[mor_sub$group=="ctrl",]$dl_avg,col="black",lwd=2,cex=2,cex.axis=2,axes=F,
     xlab="",ylab="",xaxt="n",yaxt="n",pch=21,type="n",xlim=c(0,200),ylim=c(0,25));
segments(x0=mor_sub[mor_sub$group=="ctrl",]$sholl_radius, y0=mor_sub[mor_sub$group=="ctrl",]$dl_avg - mor_sub[mor_sub$group=="ctrl",]$dl_sem, x1=mor_sub[mor_sub$group=="ctrl",]$sholl_radius, y1=mor_sub[mor_sub$group=="ctrl",]$dl_avg + mor_sub[mor_sub$group=="ctrl",]$dl_sem,col="black",lwd=2);
points(x=mor_sub[mor_sub$group=="ctrl",]$sholl_radius,y=mor_sub[mor_sub$group=="ctrl",]$dl_avg,lwd=2,cex=2,cex.axis=2,axes=F,xlab="",ylab="",xaxt="n",yaxt="n",pch=21,col="black",bg="white",type="b");
segments(x0=mor_sub[mor_sub$group=="els",]$sholl_radius, y0=mor_sub[mor_sub$group=="els",]$dl_avg - mor_sub[mor_sub$group=="els",]$dl_sem, x1=mor_sub[mor_sub$group=="els",]$sholl_radius, y1=mor_sub[mor_sub$group=="els",]$dl_avg + mor_sub[mor_sub$group=="els",]$dl_sem,col="black",lwd=2);
points(x=mor_sub[mor_sub$group=="els",]$sholl_radius,y=mor_sub[mor_sub$group=="els",]$dl_avg,lwd=2,cex=2,cex.axis=2,axes=F,xlab="",ylab="",xaxt="n",yaxt="n",pch=21,col="black",bg="black",type="b" );
axis(1,at=xscale,labels=xscalelab,tick=T,lwd=3,cex.axis=3,xpd=F,las=1,padj=0.5);
axis(2,at=yscale,labels=yscalelab,tick=T,lwd=3,cex.axis=3,xpd=F,las=1);
## dev.off();          
## Bar plot with error bars
mor_sub = morall[morall$side=="total" & morall$area == "bla" & morall$sholl_radius == 0,];
## jpeg("/mnt/storage/goofy/DATA/ELS/new/test2.jpeg",width=10,height=18,units="cm",bg="transparent",res=300)
par(bg="white",fg="black",mar=c(5,4,4,2)+c(0,3.5,0,0),lwd=4);
barplot(mor_sub$dl_tot_avg,col=c("white","black"),xlim=c(0,2.5),ylim=c(0,3000),axes=FALSE,fg="black");
Segments(x0=0.7, y0=mor_sub[mor_sub$group=="ctrl",]$dl_tot_avg - mor_sub[mor_sub$group=="ctrl",]$dl_tot_sem, x1=0.7, y1=mor_sub[mor_sub$group=="ctrl",]$dl_tot_avg + mor_sub[mor_sub$group=="ctrl",]$dl_tot_sem,col="black",lwd=4);
arrows(x0=0.7, y0=mor_sub[mor_sub$group=="ctrl",]$dl_tot_avg - mor_sub[mor_sub$group=="ctrl",]$dl_tot_sem, x1=0.7, y1=mor_sub[mor_sub$group=="ctrl",]$dl_tot_avg + mor_sub[mor_sub$group=="ctrl",]$dl_tot_sem,col="black",lwd=4,length=0.05,angle=90,code=3);
segments(x0=1.9, y0=mor_sub[mor_sub$group=="els",]$dl_tot_avg - mor_sub[mor_sub$group=="els",]$dl_tot_sem, x1=1.9, y1=mor_sub[mor_sub$group=="els",]$dl_tot_avg + mor_sub[mor_sub$group=="els",]$dl_tot_sem,col="black",lwd=4);
arrows(x0=1.9, y0=mor_sub[mor_sub$group=="els",]$dl_tot_avg - mor_sub[mor_sub$group=="els",]$dl_tot_sem, x1=1.9, y1=mor_sub[mor_sub$group=="els",]$dl_tot_avg + mor_sub[mor_sub$group=="els",]$dl_tot_sem,col="black",lwd=4,length=0.05,angle=90,code=3);
axis(2,at=c(0,1000,2000,3000),labels=as.character(c("0","1000","2000","3000")),tick=T,lwd=3,cex.axis=3,xpd=F,las=1,padj=0.5);
## dev.off();
## ----------------------------------------------
plot(0,0,lwd=2,cex=2,cex.axis=2,axes=F,xlab="",ylab="",xaxt="n",yaxt="n");

arrows(x0=mor_sub[mor_sub$group=="ctrl",]$sholl_radius, y0=mor_sub[mor_sub$group=="ctrl",]$dl_avg - mor_sub[mor_sub$group=="ctrl",]$dl_sem, x1=mor_sub[mor_sub$group=="ctrl",]$sholl_radius, y1=mor_sub[mor_sub$group=="ctrl",]$dl_avg + mor_sub[mor_sub$group=="ctrl",]$dl_sem,col="black",length=0.05,angle=90,code=3,lwd=2);

segments(x0=mor_sub[mor_sub$group=="ctrl",]$sholl_radius, y0=mor_sub[mor_sub$group=="ctrl",]$dl_avg - mor_sub[mor_sub$group=="ctrl",]$dl_sem, x1=mor_sub[mor_sub$group=="ctrl",]$sholl_radius, y1=mor_sub[mor_sub$group=="ctrl",]$dl_avg + mor_sub[mor_sub$group=="ctrl",]$dl_sem,col="black",lwd=2);
points(x=mor_sub[mor_sub$group=="ctrl",]$sholl_radius,y=mor_sub[mor_sub$group=="ctrl",]$dl_avg,lwd=2,cex=2,cex.axis=2,axes=F,xlab="",ylab="",xaxt="n",yaxt="n",pch=21,col="black",bg="white");
points(x=mor_sub[mor_sub$group=="els",]$sholl_radius,y=mor_sub[mor_sub$group=="els",]$dl_avg,col="red",lwd=2);
arrows(x0=mor_sub[mor_sub$group=="els",]$sholl_radius, y0=mor_sub[mor_sub$group=="els",]$dl_avg - mor_sub[mor_sub$group=="els",]$dl_sem,
       x1=mor_sub[mor_sub$group=="els",]$sholl_radius, y1=mor_sub[mor_sub$group=="els",]$dl_avg + mor_sub[mor_sub$group=="els",]$dl_sem,
       col="red",length=0.05,angle=90,code=3,lwd=2);
