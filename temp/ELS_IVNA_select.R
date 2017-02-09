if (exists("ivna") == 0) stop(' !!!!!!!   Excecution stopped. dataframe: "naiv" missing    !!!!!!'); 
#------------------------Select cells
select = rep(1,nrow(ivna));
ivna = cbind(ivna,"Select"=select)
expdate_colname = grep(x=colnames(ivna),pattern="ExpDate")[1] # get the name of the first column with the name ExpDate
ivna[ivna[,expdate_colname]>=20121123,"Select"]=0; # Set select = 0 if the condition is met
ivna = ivna[ivna[,"Select"]==1,]; # activate the selection
