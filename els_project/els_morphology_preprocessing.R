source("/mnt/storage/goofy/projects/codes/els/R/loadLibraries.R");
options(contrasts=c("contr.sum","contr.poly"));
## source morphology functions
source("/mnt/storage/goofy/projects/codes/els/R/els_morphology_functions.R");
## Loading full morphology data
morallo = read.csv('/mnt/storage/goofy/projects/data/els/cooked/els_morphology_all.csv',sep=",",header=TRUE,stringsAsFactors=FALSE,row.names=NULL);
## convert all NA's to zero: NA's at the last segments corresponds to zero dendritic lengths
morallo[is.na(morallo[,"dl"]),"dl"] = 0;
morall=morallo;
## morall=morall[morall$side=="apical" & morall$area=="ca1" & morall$radius <=,];
## add a new column containing actual values of ..
morall=cbind(morall,Cellid=morall$cellid); ## cell identifier
morall=cbind(morall,Radius=morall$radius); ## Sholl radii
morall=cbind(morall,Group=morall$group); ## Experimental groups
morall=cbind(morall,Side=morall$side); ## Apical/basal/full
## make some colums whose names begin with caps as factors
morall$Cellid = as.factor(morall$Cellid);
morall$Radius = as.factor(morall$Radius);
morall$Group = factor(morall$Group,levels=c("control","els"),ordered=TRUE);
morall$Side = factor(morall$Side,levels=c("apical","els","full"),ordered=TRUE);
## Total dendritic length calculation
morallt=ddply(morall,c("cellid","side"),function(x){
    id=unique(x$cellid);
    grp=unique(x$group);
    sid=unique(x$side);
    ar=unique(x$area);
    ## columns begining with caps (factorized columns)
    Grp=unique(x$Group);
    Sid=unique(x$Side);
    Ar=unique(x$Area);
    ## compute total
    tot=sum(x$dl,na.rm=TRUE);
    df_temp=data.frame("cellid"=id,"group"=grp,"side"=sid,"area"=ar,"dl"=tot);
    return(df_temp);});
## Collapse sholl radii

