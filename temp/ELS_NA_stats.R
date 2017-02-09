if (exists(els) == 0) stop(' !!!!!!!   Excecution stopped. data frame: "els" missing    !!!!!!'); # Check if DF:els exists.
#----------------------Statistics 2-way ANOVA
anovapre70peak=(aov(Peak.pre70~Treat_invivo*Treat_exvivo,els))
anovapre40peak=(aov(Peak.pre40~Treat_invivo*Treat_exvivo,els))
anovapre40tail=(aov(Tail.pre40~Treat_invivo*Treat_exvivo,els))
anovaPostPre=(aov(PostPre~Treat_invivo*Treat_exvivo,els))
anovaNAratioAmp=(aov(AmpRatio~Treat_invivo*Treat_exvivo,els))
anovaNAratioArea=(aov(AreaRatio~Treat_invivo*Treat_exvivo,els))

stat.All<-ddply(els, c("GroupName.pre70"), function(df) return(c(avg.AmpRatio=mean(df$AmpRatio), sem.AmpRatio=se(df$AmpRatio), 
									    					 avg.PeakAMPA=mean(df$Peak.pre70), sem.PeakAMPA=se(df$Peak.pre70),
														 avg.TailNMDA=mean(df$Tail.pre40), sem.TailNMDA=se(df$Tail.pre40),
														 avg.InputRes=mean(df$InputRes.pre70), sem.InputRes=se(df$InputRes.pre70),
														 avg.SeriesRes=mean(df$SeriesRes.pre70), sem.SeriesRes=se(df$SeriesRes.pre70),
														 avg.TauMem=mean(df$TauMem.pre70), sem.TauMem=se(df$TauMem.pre70),
														 avg.Age=mean(df$Age), sem.Age=se(df$Age),
														 avg.ExpDelay=mean(df$ExpDelay), sem.ExpDelay=se(df$ExpDelay),
														 avg.VRest=mean(df$VRest.pre70), sem.VRest=se(df$VRest.pre70),
														 avg.RiseTimeAMPA=mean(df$RiseTime.pre70), sem.RiseTimeAMPA=se(df$RiseTime.pre70),
														 avg.TauPeakAMPA=mean(df$TauPeak.pre70), sem.TauPeakAMPA=se(df$TauPeak.pre70),
														 avg.TauTailAMPA=mean(df$TauTail.pre70), sem.TauTailAMPA=se(df$TauTail.pre70),
														 avg.SlopeAMPA=mean(df$SlopeDirect.pre70), sem.SlopeAMPA=se(df$SlopeDirect.pre70),
														 avg.RiseTimeNMDA=mean(df$RiseTime.pre40), sem.RiseTimeNMDA=se(df$RiseTime.pre40),
														 avg.TauPeakNMDA=mean(df$TauPeak.pre40), sem.TauPeakNMDA=se(df$TauPeak.pre40),
														 avg.TauTailNMDA=mean(df$TauTail.pre40), sem.TauTailNMDA=se(df$TauTail.pre40),
														 avg.SlopeNMDA=mean(df$SlopeDirect.pre40), sem.SlopeNMDA=se(df$SlopeDirect.pre40)

														 
)))

# -----------------------------------Correlation NA ratio pre40 Peak : select groups
corrNApre40peak=lm(AmpRatio~Peak.pre40,data=els[grepl(els$GroupName.pre40,pattern="CTRL"),])
# -----------------------------------Correlation NA ratio pre40 Tail
corrNApre40tail=lm(AmpRatio~Tail.pre40,data=els[grepl(els$GroupName.pre40,pattern="CTRL"),])

# -----------------------------------Correlation NA ratio pre70 Peak : select groups
corrNApre70peak=lm(AmpRatio~Peak.pre70,data=els[grepl(els$GroupName.pre70,pattern="ELS"),])

