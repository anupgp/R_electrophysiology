#b0 = -1.41161  estimate of the intercept
#bgrowth =  -0.002072 Fixed effect of IClamp
#bX =  0.001403 Fixed effect of interaction
#b01 =   -0.37294 Fixed effect of group
#b11 =  No idea what is this!!!!
#Vint = (0.0017) variance
#Vslope = (1.193)
#Verror = (0.58640)

#==================================================================================
lmersim <-  function(ncells,b0,bGrowth,bX,b01,b11=0.005,Vint,Vslope,Verror){
   # Within levels
   within.levels = seq(150,450,25);
   # Obervation IDs
   obvid = rep(1:ncells*length(within.levels));
   #  Subject variable
   subjvar = rep(1:ncells,each=length(within.levels));
   #  Longitudinal variable
   withinvar = rep(within.levels,ncells);
   # half of the cells were treated randomly
   tmp2 = sample(0:1,ncells,replace=TRUE,prob=c(0.5,0.5));
   groupvar = tmp2[subjvar];
   # bva
   bvaset = rnorm(ncells,0,11.58);
   bva = bvaset[subjvar];
   # Random effect intercept
   S.in = rnorm(ncells,0,sqrt(Vint));
   # Random effect for slope
   S.sl = rnorm(ncells,0,sqrt(Vslope));
   # Observation level error
   eps = rnorm(ncells*within.levels,0,sqrt(Verror));
   # Create outcome as product of specified model
   paramvalue = b0 + b01*bva + bGrowth*withinvar + bX*groupvar*withinvar + S.in[subjvar] + S.sl[subjvar]*withinvar + eps[obvid];
   # Put it into a dataframe
   fkdata = data.frame(obvid,subjvar,groupvar,withinvar,paramvalue);
   return(fkdata);
   }
