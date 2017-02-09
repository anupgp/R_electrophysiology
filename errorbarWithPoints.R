## Plots errorbar with points!
gtest=ggplot(data=avgpre7040, aes(x=GroupName, y=Tail.pre40, fill=GroupName)) + 
	geom_bar(data=avgpre7040,position=position_dodge())+
    geom_errorbar(data=avgpre7040,aes(ymin=Tail.pre40-se, ymax=Tail.pre40+se),
    width=0.2,    # Width of the error bars
    position=position_dodge(0.9))




