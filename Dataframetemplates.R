# Creates a data frame template for storing all measurements from the accomodation traces
createdf <- function(){
	ahpdfcolnames = c("filename","group","traceid","rmp","ires","sres","tau","vm",
	"peak","peakx","peaky","dip","dipx","dipy","mAHPp","mAHPt","mAHPa","sAHPp","sAHPt","sAHPa")
	ahpdfcollist = list("","",0,0,0,0,matrix(0,nrow=1,ncol=2),0,0,matrix(0,nrow=1,ncol=60),
	matrix(0,nrow=1,ncol=60),0,matrix(0,nrow=1,ncol=60),matrix(0,nrow=1,ncol=60),0,0,0,0,0,0)
	names(ahpdfcollist) = ahpdfcolnames
	ahpdf = data.frame(ahpdfcollist,stringsAsFactors=F)
return(ahpdf)
}
#=========================================================================
