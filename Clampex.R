# This R script contains a function to load ".abf" files containing episodic data with multiple sweeps and more than one channels
abfload <- function(fn,path,chName)
{
 headpar <- list(fFileVersionNumber = list(offset=4,type= "numeric",size=4,value = -1),  
 		nOperationMode = list(offset=8,type= "integer",size=2,value = -1), 
 		lActualAcqLength = list(offset=10,type ="integer",size=4,value = -1),
 		nNumPointsIgnored = list(offset=14,type ="integer",size=2,value = -1),
 		lActualEpisodes = list(offset=16,type="integer",size=4,value= -1),
 		lFileStartTime = list(offset=24,type="integer",size=4,value= -1),
 		lDataSectionPtr = list(offset=40,type="integer",size=4,value = -1),
		lSynchArrayPtr = list (offset=92,type="integer",size=4,value = -1),
 		lSynchArraySize = list(offset=96,type="integer",size=4, value =-1), 
		nDataFormat = list(offset=100,type="integer",size=2, value = -1),            
  		nADCNumChannels = list(offset=120,type="integer",size=2, value = -1),
  		fADCSampleInterval = list(offset=122,type="numeric",size=4, value= -1), 
 		fSynchTimeUnit = list(offset=130,type="numeric",size=4,value=-1),
		lNumSamplesPerEpisode = list(offset=138,type="integer",size=4,value= -1),        
 		lPreTriggerSamples = list(offset=142,type="integer",size=4,value = -1),        
 		lEpisodesPerRun = list(offset=146,type="integer",size=4,value = -1),        
 		fADCRange = list(offset=244, type="numeric",size=4,value = -1),
 		lADCResolution = list(offset=252,type= "integer",size=4, value=-1),
		nFileStartMillisecs = list(offset=366,type= "integer",size=2, value =-1),
 		nADCPtoLChannelMap = list(offset=378,type= "integer",size=2,value= matrix(-1,nrow=1,ncol=16)),
		nADCSamplingSeq = list(offset=410,type= "integer",size=2,value=matrix(-1,nrow=1,ncol=16)),
 		sADCChannelName = list(offset=442,type="character", size=10,value=matrix("",nrow=1,ncol=16)), #size = 10
 		sADCChannelunits = list(offset=602,type="character", size=8,value=matrix("",nrow=1,ncol=16)), # size = 8
		fADCProgrammableGain = list(offset=730,type= "numeric", size=4,value=matrix(-1,nrow=1,ncol=16)),
 		fInstrumentScaleFactor = list(offset=922,type= "numeric",size=4,value=matrix(-1,nrow=1,ncol=16)),
 		fInstrumentOffset = list(offset=986,type= "numeric",size=4,value=matrix(-1,nrow=1,ncol=16)),
 		fSignalGain = list(offset=1050,type="numeric",size=4,value=matrix(-1,nrow=1,ncol=16)),
 		fSignalOffset = list(offset=1114,type= "numeric", size = 4,value=matrix(-1,nrow=1,ncol=16)),
 		nTelegraphEnable =list(offset=4512,type="integer",size=2,value=matrix(-1,nrow=1,ncol=16)),
 		fTelegraphAdditGain = list(offset=4576,type="numeric",size=4,value=matrix(-1,nrow=1,ncol=16)))
cdir <- getwd()
setwd(path)
fid <- file(fn,"rb")
# Reading the header information
m <- 1
while(m<=length(headpar))
{
	seek(fid,headpar[[m]]$offset)
	if (length(headpar[[m]]$value) == 1){
		data <- readBin(fid,what=headpar[[m]]$type,size=headpar[[m]]$size,endian="little",n=1)
		headpar[[m]]$value <- data
	}
	if (length(headpar[[m]]$value) > 1 && pmatch(headpar[[m]]$type,"character",nomatch=0)==0){
		data <- readBin(fid,what=headpar[[m]]$type,size=headpar[[m]]$size,endian="little",n=length(headpar[[m]]$value))
		headpar[[m]]$value <- data
	}
	if(length(headpar[[m]]$value) > 1 && pmatch(headpar[[m]]$type,"character",nomatch=0)!=0){
		n <- 1
		while(n<= length(headpar[[m]]$value))
		{	data[n] <- readChar(fid,headpar[[m]]$size)
			headpar[[m]]$value[n] <- data[n]
			n <- n+1
	}	}
	m <- m+1
}
# End of reading header information

recChIdx <- headpar$nADCSamplingSeq$value[1:headpar$nADCNumChannels$value] # array containing the channel id of each sampled channel
recChInd <- c(1: length(recChIdx)) # array of channel index starting 1 for the first channel  
recChNames <- c(headpar$sADCChannelName$value[recChIdx+1]) # getting channel names
print (c(recChIdx,recChInd,recChNames))

addGain <- headpar$nTelegraphEnable$value * headpar$fTelegraphAdditGain$value
addGain[addGain==0] <- 1
print(c(headpar$nTelegraphEnable$value,headpar$fTelegraphAdditGain$value,addGain))

blockSz <- 512
dataSz <- 2
dataType <- "integer"

headOffset <- headpar$lDataSectionPtr$value*blockSz+headpar$nNumPointsIgnored$value*dataSz #Gets the offset position of data 
si <- headpar$fADCSampleInterval$value*headpar$nADCNumChannels$value # Gets the sampling interval in micro sec
print (c(headOffset,si))

nSweeps <- headpar$lActualEpisodes$value
sweeps <- c(1:headpar$lActualEpisodes$value) # an array with sweep indices 
print (c(nSweeps,sweeps))


startPt <-0
dataPts <- headpar$lActualAcqLength$value
dataPtsPerChannel <-dataPts/headpar$nADCNumChannels$value
dataPtsPerChannelPerSweep <-dataPtsPerChannel/headpar$lActualEpisodes$value
dataPtsPerSweep <- dataPtsPerChannelPerSweep*headpar$nADCNumChannels$value
print(c(startPt,dataPts,dataPtsPerChannel,dataPtsPerChannelPerSweep,dataPtsPerSweep))

seek(fid,startPt*dataSz+headOffset)
print(seek(fid))

data <- array(-1, dim=c(dataPtsPerChannelPerSweep,nSweeps,length(recChInd)))
print(dim(data))
sweepStartPts <- ((sweeps-1)*dataPtsPerSweep)*dataSz+headOffset
print(list("sweepStartPts",sweepStartPts))
# Read sweep data for all channels  
i <- 1
while (i <= nSweeps)
{
	seek(fid,sweepStartPts[i])
	tmpd <- readBin(fid,what="integer",size=2,endian="little",n=dataPtsPerSweep)
	tmpd <- matrix(tmpd,nrow=dataPtsPerChannelPerSweep,ncol=length(recChInd),byrow=TRUE) # the data per sweep is split into channels in each column
	data[,i,] <- tmpd # data is acquired per sweep for all the channels
	i <- i+1
}

# scale the data channels
i <- 1
while (i <= length(recChInd))
{
	print(c(length(recChInd),i))
	print(c(headpar$fInstrumentScaleFactor$value[recChIdx[i]+1],headpar$fSignalGain$value[recChIdx[i]+1],
					headpar$fADCProgrammableGain$value[recChIdx[i]+1],addGain[recChIdx[i]+1]))
	print(c(headpar$fADCRange$value,headpar$lADCResolution$value,headpar$fInstrumentOffset$value[recChIdx[i]+1],headpar$fSignalOffset$value[recChIdx[i]+1]))
	data[,,i] <- data[,,i]/(headpar$fInstrumentScaleFactor$value[recChIdx[i]+1]*headpar$fSignalGain$value[recChIdx[i]+1]*
			   headpar$fADCProgrammableGain$value[recChIdx[i]+1]*addGain[recChIdx[i]+1])*
			 headpar$fADCRange$value/headpar$lADCResolution$value+ (headpar$fInstrumentOffset$value[recChIdx[i]+1]+headpar$fSignalOffset$value[recChIdx[i]+1])
#	data[,,i] <- round(data[,,i],2) # data rounded to 2 decimal points
	i <- i+1
}
#convert the data into timeseries of the specified channel
i <- 1
while (i <= length(recChNames))
{
	if(pmatch(chName,recChNames[i],nomatch=0)!=0)
	{
	data <- ts(data[,,i],start=0,deltat=si*1E-06)
	print(c("Channel aqcuired =",recChNames[i]),quote=FALSE)
	}
	i <- i +1
}
return(data)

# close the file and remove all variables
close(fid,type="rb")
#rm(list = ls())
setwd(cdir)

}
#============================================================================
#This Function generates a low-pass guassian filter for a given radius
#Author Anup Pillai
#Date SEP092011
 gkernal <- function(radius)
{
#Filter function
 width <- round(radius)
 gkernal <- rep(0,width*2+1)
 sigma <- radius/3
 norm <- 1/(sqrt(2*pi)*sigma)
 coeff <- 2*sigma*sigma
 x <- seq(from=-width,to=width,by=1)
 gaussian <- c(expression(norm*exp(-x*x/coeff)))
 kernal <- eval(gaussian)
 kernal <- kernal/sum(kernal)
 return(kernal)
}
#=============================================================================
