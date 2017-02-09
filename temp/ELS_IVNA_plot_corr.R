if (exists("ivna") == 0) stop(' !!!!!!!   Excecution stopped. dataframe: "naiv" missing    !!!!!!'); 
# Stats for IVNA correlation
corampratio_els=cor.test(~ AmpRatio+RevPeakCor.iv,alternative="two.sided",method="pearson",data=ivna[grepl(pattern="ELS",ivna$GroupName.pre70),])
corampratio_control=cor.test(~ AmpRatio+RevPeakCor.iv,alternative="two.sided",method="pearson",data=ivna[grepl(pattern="CTRL",ivna$GroupName.pre70),])
# Correlation plot NA versus Rev. potential

elscoeff = coef(lm(AmpRatio~RevPeakCor.iv, data = ivna[grepl(ivna$GroupName.pre70,pattern="ELS"),]))
controlcoeff = coef(lm(AmpRatio~RevPeakCor.iv, data = ivna[grepl(ivna$GroupName.pre70,pattern="CTRL"),]))
plot.corivna = qplot(RevPeakCor.iv,AmpRatio,data=ivna)+
		#geom_smooth(method="lm",se=FALSE, color="black", formula = y ~ x)+
		geom_abline(intercept = controlcoeff[1], slope = controlcoeff[2],colour = "grey", size = 1.5)+
		geom_abline(intercept = elscoeff[1], slope = elscoeff[2],colour = "black", size = 1.5)+
		layer(data=ivna,mapping=aes(x=RevPeakCor.iv,y=AmpRatio,color=Treat_invivo,group=Treat_invivo),size=5,geom="point")+
		#annotate(geom="text",x=11,y=1.2,label = "Adj.R-sqr = -0.1",size=5,fontface="italic",color="black",hjust=0)+
		#annotate(geom="text",x=11,y=1.13,label = "p = 0.95",size=5,fontface="italic",color="black",hjust=0)+
		theme(axis.text.y = element_text(size=30, hjust=0.5,angle=0,color="black"), axis.title.y = element_text(size=18, angle=90,vjust=0.25),
			    axis.text.x  = element_text(angle=0, hjust=0.5, size=30,face="plain",color ="black"),axis.title.x = element_text(size=25, angle=0,vjust=0.25), plot.margin = unit(c(0.4, 0.7, 0, 0), "cm"))+
		scale_colour_manual("", breaks = levels(factor(ivna$Treat_invivo)), values= c("grey","black") )
		#scale_x_continuous('Epsc Peak Reversal Potential (mV) ') + scale_y_continuous('Epsc Amplitude Ratio (NMDA Tail /AMPA Peak) ')+
		#theme(legend.title = element_text(colour="blue", size=16, face="bold",hjust=1))+
		#theme(legend.text = element_text(colour="black", size = 16, face = "plain"))
plot.corivna=plot.corivna+theme(panel.background = element_rect(fill='white', colour='black'))
plot.corivna = plot.corivna+#theme_bw()+
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
coord_cartesian(ylim=c(0, 0.8),xlim=c(-40,50))

tiff("~/DATA/ELS/ELS_PkRevNACor",width=17,height=17,units="cm",compression=c("jpeg"),bg="white",res=300)

