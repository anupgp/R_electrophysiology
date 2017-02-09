astropub=read.csv("~/DATA/IISER_Job/PubmedSearchTips/AstrocytePubsData.csv");
astro1=reshape(direction="long",varying=colnames(astropub)[!colnames(astropub)=="year"],v.names="pubs",times=colnames(astropub)[!colnames(astropub)=="year"],data=astropub,timevar="searchid");
astro2=ddply(astro1,c("searchid"),function(x){cbind(astro1,pubspercent=(x[,"pubs"]/x[,"pubs"][1])*100)});
astro2$pubspercent=as.numeric(astro2$pubspercent);
astro1=cbind(astro1,pubspercent=astro2$pubspercent);

jpeg(file="/home/goofy/DATA/IISER_Job/Astrocytes/PubNeuronAstrocyte.jpg",width=12,height=12,units="cm",bg="white",res=300);
plot(astropub$year,astropub$neuronastrocyte,xlab="Years",ylab="No. of publications",type="o",main="NueronAstrocyte",col="blue",lwd=2)
dev.off();
