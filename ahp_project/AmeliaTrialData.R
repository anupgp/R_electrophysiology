# Create a dataframe to contain the subject name and the repeated measures factor Stimulus intensity
mydata=data.frame(SubjectName=sort(rep(paste("Subject",1:8,sep="_"),8)),StimIntensity=(rep(seq(100,800,by=100))));
# Add the between subjects variable Group 
mydata = cbind(mydata,Group=sort(rep(c("Neutral","Happy"),32),decreasing=T));
# Creates the Response time values for first group : "Neutral"
i=0;a=0;while (i < 4) {a[((i*8)+1):((i*8)+8)]=sort(rnorm(mean=10+(i*10):20+(i*10),1,n=8)); i = i+1;};
# Creates the Response time values for the second  group : "Happy"
i=0;b=0;while (i < 4) {b[((i*8)+1):((i*8)+8)]=sort(rnorm(mean=10+(i*10):20+(i*10),1,n=8))*c(1,1,1,1,1,1.5,1.75,2.2); i = i+1;};
# Add the data to the RT column in mydata dataframe
mydata=cbind(mydata,"RT"=c(a,b));
# Creates a second dataframe with missing data (n=15)
mydataMissing = mydata;
mydataMissing[runif(n=15,min=1,max=64),"RT"] = NA;
lineplot.CI(StimIntensity,RT,group=Group,data=mydata)
mydata.model.rs.full  = lmer(RT ~ Group*StimIntensity + (1+StimIntensity|SubjectName),data=mydata);
mydata.model.rs.null  = lmer(RT ~ Group+StimIntensity + (1+StimIntensity|SubjectName),data=mydata);
print(anova(mydata.model.rs.full,mydata.model.rs.null))


