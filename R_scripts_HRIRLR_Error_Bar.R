#==========================================================================
i = 1
while (i < 100)
{names
x$Animal_ID[i]=as.character(x$Animal_ID[i])

j=1
while (j<20)
{
if(charmatch((x$Animal_ID[i]),codes$UVAcode[j],nomatch=0)!=0)
 x$group[i] <- codes$groups[j]
print(c("Match!  : ",x$Animal_ID[i],codes$UVAcode[j]))
j=j+1
}
i=i+1
}
#==========================================================================
errorbar <- function(x,y,upper,lower=upper,length=0.1,...)
{
	if(length(x) != length(y) | length(y) != length(upper) | length(lower) != length(upper))
	stop("Vectors muust be of the same length")
	arrows(x,y+upper,x,y-lower,angle=90,code=3,length=length,...)

}
#===========================================================================
tiff("F:/DATA/PostDoc1_Marian/HRIRLR/R_HRIRLR/Spines2_HRIRLR.tiff",width=8,height=15,units="cm",compression=c("none"),bg="white",res=150)
barsem <- barplot(x2.means,ylim=c(0,12),col = c("blue","grey","red"),xlab="groups",axis.lty=1,ylab="Spines/15 microns",beside=T,space=c(0.3,0.2),border=F,offset=0.3)
errorbar(barsem,x2.means,x2.sem,col="black")
dev.off()
#=================================================================================