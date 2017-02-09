#elsCorr = els[grepl(els$GroupName.pre40,pattern="ELS"),];
elsCorr = els;
elscoeff = coef(lm(Tail.pre40~ AmpRatio, data = els[grepl(els$GroupName.pre40,pattern="ELS"),]))
controlcoeff = coef(lm(Tail.pre40~ AmpRatio, data = els[grepl(els$GroupName.pre40,pattern="CTRL"),]))
plot.NApre40tail = qplot(AmpRatio,Tail.pre40,data=elsCorr)+
		#geom_smooth(method="lm",se=FALSE, color="black", formula = y ~ x)+
		geom_abline(intercept = controlcoeff[1], slope = controlcoeff[2],colour = "grey", size = 1.5)+
		geom_abline(intercept = elscoeff[1], slope = elscoeff[2],colour = "black", size = 1.5)+
		layer(data=elsCorr,mapping=aes(x=AmpRatio,y=Tail.pre40,color=Treat_invivo,group=Treat_invivo),size=5,geom="point")+
		#annotate(geom="text",x=11,y=1.2,label = "Adj.R-sqr = -0.1",size=5,fontface="italic",color="black",hjust=0)+
		#annotate(geom="text",x=11,y=1.13,label = "p = 0.95",size=5,fontface="italic",color="black",hjust=0)+
		theme(axis.text.y = element_text(size=25, hjust=0.5,angle=0,color="black"), axis.title.y = element_text(size=18, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=25,face="plain",color =" black"),axis.title.x = element_text(size=25, angle=0,vjust=0.25), plot.margin = unit(c(0.4, 0.7, 0, 0), "cm"))+
		scale_colour_manual("", breaks = levels(factor(elsCorr$Treat_invivo)), values= c("grey","black") )
		#scale_x_continuous('Epsc Peak Reversal Potential (mV) ') + scale_y_continuous('Epsc Amplitude Ratio (NMDA Tail /AMPA Peak) ')+
		#theme(legend.title = element_text(colour="blue", size=16, face="bold",hjust=1))+
		#theme(legend.text = element_text(colour="black", size = 16, face = "plain"))
plot.NApre40tail=plot.NApre40tail+theme(panel.background = element_rect(fill='white', colour='black'))
plot.NApre40tail = plot.NApre40tail+#theme_bw()+
theme(
    plot.background = element_blank()
   ,panel.grid.major = element_blank()
   ,panel.grid.minor = element_blank()
   ,panel.border = element_blank()
   ,panel.background = element_blank(),
	#axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
	axis.line = element_line(color = 'black'),
        axis.title.y=element_blank(),legend.position="none"
  ) +
coord_cartesian(ylim=c(0, 200),xlim=c(0,0.8))

#tiff("~/DATA/ELS/ELSNAper40tail",width=17,height=17,units="cm",compression=c("jpeg"),bg="white",res=300)


plot.NApre40peak = qplot(AmpRatio,Peak.pre40,data=elsCorr)+geom_smooth(method=lm,fullrange=TRUE)+
		layer(data=elsCorr,mapping=aes(x=AmpRatio,y=Peak.pre40,color=GroupName.pre40),size=5,geom="point")+
		#annotate(geom="text",x=11,y=1.2,label = "Adj.R-sqr = -0.1",size=5,fontface="italic",color="black",hjust=0)+
		#annotate(geom="text",x=11,y=1.13,label = "p = 0.95",size=5,fontface="italic",color="black",hjust=0)+
		theme(axis.text.y = element_text(size=18, hjust=0.5,angle=0), axis.title.y = element_text(size=18, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=18,face="plain"),axis.title.x = element_text(size=18, angle=0,vjust=0.25))+
		scale_colour_manual("", breaks = levels(factor(elsCorr$GroupName.pre40)), values= c("darkgrey","black") )+
		#scale_x_continuous('Epsc Peak Reversal Potential (mV) ') + scale_y_continuous('Epsc Amplitude Ratio (NMDA Tail /AMPA Peak) ')+
		theme(legend.title = element_text(colour="blue", size=16, face="bold",hjust=1))+
		theme(legend.text = element_text(colour="black", size = 16, face = "plain"))
plot.NApre40tpeak=plot.NApre40peak+theme(panel.background = element_rect(fill='white', colour='black'))
