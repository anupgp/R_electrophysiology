#==============================================================================================================
# Function to artificially increase 'n' to compute power for different sample size
FlmerpowerN  <-  function(df1,pname,lmermod,nsim=100,N){
   # N: no.of runs of the function with mutiples of n from the given dataframe - df1
   # lmer: linear mixed factor model as obtained from lmer
   # df1 dataframe which will be simulated using lmer and rbinded N times
   # n: no.of simulations per each run of N
   # pname: the parameter name
   # Function outputs a dataframe with three colums - totalsamplesize,power main effect, power interaction
   fileid = "FileID";
   dfnames=names(df1);
   dfnamesnoparam=dfnames[dfnames!=pname];
   dfall = df1[,c(dfnamesnoparam,pname)];
   # Make dataframe to hold the power for each run
   powerN = data.frame("SampleSize"=NA,"PowerMain"=NA,"PowerInt"=NA);
   # Calculate power for the orginal dataset
   powerN[1,"SampleSize"]=nrow(dfall);
   powerN[1,"PowerMain"]= sum((Flmerpower(dfall,lmermod,nsim))$pvalues$MEGroupName < 0.05);
   powerN[1,"PowerInt"]= sum((Flmerpower(dfall,lmermod,nsim))$pvalues$Inter < 0.05);
   for(i in 2:N){
      # Generate the dataframe with N times the orginal dataframe
      simulateSeed = i;
      yvals = simulate(lmermod,nsim=1,seed=simulateSeed,use.u=T);
      names(yvals)=pname;
      dfsim = cbind(df1[,dfnamesnoparam],yvals);
      dfsim[,fileid]=paste(df1[,fileid],"sim_1",sep="_");
      dfall = rbind(dfall,dfsim);
      # Calculate power for each simulated dataset with increasing n (n=multiples of the orginal dataset)
      powerN[i,"SampleSize"]=nrow(dfall);
      powerN[i,"PowerMain"]= sum((Flmerpower(dfall,lmermod,nsim))$pvalues$MEGroupName < 0.05);
      powerN[i,"PowerInt"]= sum((Flmerpower(dfall,lmermod,nsim))$pvalues$Inter < 0.05);
      }
   return(powerN);
   }
