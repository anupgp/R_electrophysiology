if (exists("els") == 0) stop(' !!!!!!!   Excecution stopped. data frame: "els" missing    !!!!!!'); # Check if DF:els exists.
#------------------------Select cells
select = rep(1,nrow(els));
els = cbind(els,"Select"=select)
expdate_colname = grep(x=colnames(els),pattern="ExpDate")[1] # get the name of the first column with the name ExpDate
els[els[,expdate_colname]>=20121123,"Select"]=0; # Set select = 0 if the condition is met
els = els[els[,"Select"]==1,]; # reset all the selections = include cells
