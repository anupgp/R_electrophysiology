#------------------------Select cells
select = rep(1,nrow(els));
els = cbind(els,"Select"=select)
els[els[,"ExpDate.pre70"]>=20121123,"Select"]=0;
#els[els[,"ExpDate.pre70"]==20121127,"Select"]=0;
#els[els[,"ExpDate.pre70"]>=20121130,"Select"]=0;
#els[els[,"Age"]<=50,"Select"]=0;
#els[els[,"Age"] <=60,"Select"] = 0;
els = els[els[,"Select"]==1,];
