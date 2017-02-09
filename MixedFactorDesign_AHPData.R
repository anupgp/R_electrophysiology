#--------------------------------------- Some Functions 
doanova <- function(x, y) {return(anova(x, y))}; # FixWeirdAnovaBehavior
rep.row<-function(x,n){matrix(rep(x,each=n),nrow=n)}; # Repates Rows
rep.col<-function(x,n){matrix(rep(x,each=n), ncol=n, byrow=TRUE)}; # Repeates Columns
# Function that takes a dataframe and operates a function on each block obtained from the levels of each factor variables (2)
fun.block <- function(df,var1,var2,param,funname,...) # var1: bw factor, var: within factor
	{ 
	var1.levels = unique(as.character(df[,var1]));
	var2.levels = unique(as.character(df[,var2]));
	df.mat=matrix(0,length(var1.levels),length(var2.levels));
	i=1;  
	FUN = get(funname);
	while( i<= length(var1.levels) ) # Between subject loop: GroupName
		{
		j =1; 
		while( j <= length(var2.levels) ) # Within subject loop: IClamp 
			{
			df.mat[i,j]= FUN(df[ (df[,var1] == var1.levels[i] & df[,var2] == var2.levels[j]),param],...);
			j = j+1;		
			}	
		i = i+1;
		}
	return(df.mat);
	}
#-----------------------------------
model.param = c("PeakAhpMf");
model.subject = "FileID";
#Getting the levels for each factor
wi.levels =  sort(unique(ahp$IClamp));
bw.levels = unique(ahp$GroupName);
#Add escape characters to between levels
bw.levels=gsub(pat="[+]",rep='\\\\+',bw.levels)
# Adding NA where the values are missing for the ahpsmall for the within & between dataframe
#ahpsmall = ahp[,c(model.subject,"GroupName","IClamp",model.param)];
ahpsmall = ddply(ahp,model.subject,function(x){val=rep(0,length(wi.levels));val[which(wi.levels%in%x[,"IClamp"])]=x[,model.param];
				data.frame(FileID = rep(as.character(x[1,model.subject]),length(wi.levels)),IClamp = wi.levels, 
				GroupName= rep(as.character(x[1,"GroupName"]),length(wi.levels)), Param = val)});
#ahpbog dataset with more params
ahpbig = ddply(ahp,model.subject,function(x){
				val1=rep(NA,length(wi.levels));val1[which(wi.levels%in%x[,"IClamp"])]=x[,"PeakAhpMf"]; #PeakAhpMf
				val2=rep(NA,length(wi.levels));val2[which(wi.levels%in%x[,"IClamp"])]=x[,"PeakAhpSf"]; #PeakAhpSf
				val3=rep(NA,length(wi.levels));val3[which(wi.levels%in%x[,"IClamp"])]=x[,"InputResa"]; #InputResa
				data.frame(FileID = rep(as.character(x[1,model.subject]),length(wi.levels)),IClamp = wi.levels, 
				GroupName= rep(as.character(x[1,"GroupName"]),length(wi.levels)), 
				PeakAhpMf = val1,#PeakAhpMf
				PeakAhpSf = val2,#PeakAhpSf
				InputResa = val3#InputResa
				)});
#--------------------------------------------------
ahpsmall$Param[which(ahpsmall$Param == 0)] = NA;
ahpsmall = ahpsmall[,c("FileID","GroupName","IClamp","Param")];
ahpsmall$FileID = as.character(ahpsmall$FileID);
# Multiple Imputations using Amelia
ahpsmall.mi = amelia(x=ahpsmall,ts="IClamp",cs="GroupName",idvars="FileID",polytime=1,bounds=matrix(c(4,-Inf,0),1,3)); # cs="GroupName"
#=========================================================
models.rs.full =  lapply(paste("imp",1:ahpsmall.mi[["m"]],sep=""), function(x) 
	{lmer(Param ~ GroupName*IClamp + (1+IClamp|FileID),data=ahpsmall.mi$imputations[[x]],REML= F);})
coef.full = t(sapply(models.rs.full,function(x){fixef(x)} )); 
models.rs.red = lapply(paste("imp",1:ahpsmall.mi[["m"]],sep=""), function(x) 
	{lmer(Param ~ IClamp+ (GroupName:IClamp) + (1+IClamp|FileID),data=ahpsmall.mi$imputations[[x]],REML= F);})
for (i in 1: length(models.rs.full))
	{
	model.rs.full = models.rs.full[[i]];
	model.rs.red = models.rs.red[[i]];	
	model.anova[[i]] = doanova(model.rs.full,model.rs.red);
	Pr.anova[i] = model.anova[[i]]$Pr[2];
	#readline();
	 }
#========================================================
#Mean of parameter estimates across imputation
Qmean = colMeans(coef.full);
se.full = t(sapply(models.rs.full,function(x){sqrt(diag(vcov(x)) )}));
# Within imputation variaince: mean of standard errors
SE.within = colMeans(se.full); 
names(SEwithin) = names(Qmean);
# Between imputation variance
N.imp = dim(coef.full)[1]; 
Var.bw = colSums((coef.full-rep.row(Qmean,5))^2)/(N.imp-1);
# Total Variance
Var.tot = SE.within + ( (1+1/ N.imp) * Var.bw);
# T.Test between imputations
t.imp = Qmean/sqrt(Var.tot);
# degree of freedom between imputations
df.imp = (N.imp-1)*(1+( (N.imp*SE.within)/((N.imp+1)*Var.bw)) );
Pr.imp = pt(t.imp,df.imp);
# Rate of missing information
r.imp = ((1+(1/N.imp))*Var.bw)/SE.within;
gamma.imp  =  ((r.imp+2)/(df.imp+3))/(r.imp+1);















