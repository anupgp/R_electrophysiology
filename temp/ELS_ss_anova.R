# To adhere to the sum-to-zero convention for effect weights, you should always do this before running anovas in R
options(contrasts=c("contr.sum","contr.poly"))
# ANOVA on nss70100amp
#Perfrom two-way mixed anova with one within(StimNum) & one between factors(GroupName)
#nss70100amp_aov = aov(Peak ~ StimNum * GroupName+ Error(CellID/StimNum ), data=nss70100amp)
#Perfrom two-way mixed anova with one within(StimNum) & two between factors(Treat_invivo & Treat_exvivo)
#nss70100amp_aov = aov(Peak ~ StimNum * Treat_invivo * Treat_exvivo + Error(CellID/StimNum ), data=nss70100amp);
nss70100pamp_aov = aov(Peak ~ Treat_invivo * Treat_exvivo * StimNum + Error(CellID), data=nss70100pamp);
