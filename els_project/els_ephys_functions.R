## Function to remove outliers 'n' times Sdev == NA with one between subject multifactor design
dfOutlierNA <- function(idvar,paramname,groups,sdtimes,DF)
    {
        ##============= Checks============
        if(length(groups) != length(sdtimes))
            {
                print("length of groups not equal to length of sdtimes");
                return();
            }
        if ((!paramname%in%names(DF)) & (!groups[1]%in%names(DF)))
            {
                print("Paramname or Groupname is not present in the dataframe");
                return();
            }
        ddply.outlierNA <- function(x,idvar,paramname,sdtimes)
            {
                new_vals = rep(NA,dim(x)[1]);
                vals = x[,paramname];
                indxVals = which(vals >= (mean(vals,na.rm=T)-(sdtimes*sd(vals,na.rm=T))) & vals <= (mean(vals,na.rm=T) + (sdtimes*sd(vals,na.rm=T))) );
                new_vals[indxVals] = vals[indxVals];
                xout=x;
                xout[,paramname] = new_vals;
                return(xout);
            }
        df.out = ddply(DF,c(groups),ddply.outlierNA,idvar,paramname,sdtimes);
        return(df.out);
    }
## ==========================================================================
## Function to add to compute delta for NMDA/AMPA ratio (suggestion by Matthias Smidt)
## This function is not yet fully done 
normalize_nar <- function(indat)
    {
        outdat = ddply(indat,c("CellID"),function(x)
            {
                nar_am_mgfree_norm = c();
                nar_am_mgfreeless_norm = c();
                nar_am_depolnorm = c();
                
                nar_am_mgfree_norm = c(nar_am_mgfree_norm,x[,"narAMmgfree"]/mean(x[x$GroupOrd == "CTRL+VEH","narAMmgfree"],na.rm=T));
                nar_am_mgfreeless_norm = c(nar_am_mgfreeless_norm,x[,"narAMmgfreeless"]/mean(x[x$GroupOrd=="CTRL+VEH","narAMmgfreeless"],na.rm=T));
                nar_am_depolnorm = c(nar_am_depolnorm,x[,"narAMdepol"]/mean(x[x$GroupOrd=="CTRL+VEH","narAMdepol"],na.rm=T));

                browser();

                dfout = as.data.frame(cbind(x,narAMmgfreenorm = nar_am_mgfree_norm,

                    narAMmgfreelessnorm = nar_am_mgfreeless_norm,
                    narAMdepolnorm = nar_am_depolnorm))
                    
                
                return(dfout);
            })
            return(outdat);
        }                

                ## ===========================================================================
## Function to process EPSC data
EPSC_process <- function(EPSC,less_sweeps)
    {
        EPSC = ddply(EPSC,c("CellID"),function(x)
            {
                allrecmodes=c("EPSC70MinusPre","EPSC40Plus","EPSC70MinusCNQMgFree","EPSC70MinusPost","EPSC70MinusCNQMgFreeAPV","EPSCIV");
                arecmode = allrecmodes[allrecmodes%in%unique(x$RecMode)];
                print(arecmode);
                timeindex=c();
                indices=c();
                count=c();
                totalsweepcount = c();

                avg_am_peaks=c();
                avg_am_peak=c();
                avg_am_tails=c();
                avg_am_tail=c();
                avg_am_peakless=c();
                
                avg_am_cvpeaks=c();
                avg_am_cvpeak=c();
                avg_am_cvtails=c();
                avg_am_cvtail=c();
                
                avg_ar_peaks=c();
                avg_ar_peak=c();
                avg_ar_tails=c();                
                avg_ar_tail=c();
                avg_ar_peakless=c();

                avg_am_ampa=NA;
                avg_am_nmda_neg=NA;
                avg_am_nmda_pos=NA;
                avg_am_ampaless=NA;
                avg_am_nmdaless_neg=NA;

                avg_ar_ampa=NA;
                avg_ar_nmda_neg=NA;
                avg_ar_nmda_pos=NA;
                avg_ar_ampaless=NA;
                avg_ar_nmdaless_neg=NA;

                avg_am_prepost=NA;
                avg_am_prepostless=NA;
                
                nar_am_depol=NA;      ## NA Ratio from peak 'am'plitude
                nar_am_mgfree=NA;     ## NA Ratio from peak 'am'plitude
                nar_am_mgfreeless=NA; ## NA Ratio from peak 'am'plitude

                nar_ar_depol=NA;      ## NA Ratio from 'ar'ea
                nar_ar_mgfree=NA;     ## NA Ratio from 'ar'ea
                nar_ar_mgfreeless=NA; ## NA Ratio from 'ar'ea

                avg_am_cv_ampa = NA;
                avg_am_cv_nmda_neg = NA;
                avg_am_cv_nmda_pos = NA;
                
                avg_sr=c();
                avg_srs=c();
                avg_ir=c();
                avg_irs=c();
                avg_cap=c();
                avg_caps=c();
                avg_rt=c();
                avg_rts=c();
                avg_slope=c();
                avg_slopes=c();
                avg_taupeak=c();
                avg_taupeaks=c();
                avg_tautail=c();
                avg_tautails=c();
                avg_taufull=c();
                avg_taufulls=c();
                
                cat(c(unique(x$CellID),unique(x$BrainRegion),unique(x$ExpType),arecmode,"\n"));
                for (i in 1:length(arecmode)){
                    num_rows = dim(x[x[,"RecMode"] == arecmode[i],])[1];
                    ## browser();
                    if(i==1){
                        startval = -min(num_rows,20);
                        endval = -1;
                        erow = num_rows;
                        srow = max(erow-20+1,1);
                    }
                    if(i==2){
                        startval = 0;
                        endval = min(num_rows,20) - 1;
                        erow = erow + num_rows;
                        srow = max(erow-20,erow-num_rows) + 1;
                    }
                    if(i==3){
                        startval = 20 * (i-2);
                        endval = startval + min(num_rows,20) - 1;
                        erow = erow + num_rows;
                        srow = max(erow-20,erow-num_rows) + 1;
                    }
                    if(i>3){
                        startval = 20 * (i-2);
                        endval = startval + min(num_rows,20) - 1;
                        row = erow + num_rows;
                        srow = max(erow-20,erow-num_rows) + 1;
                    }
                    print(c(unique(x[,"CellID"]),arecmode[i],as.character(c(num_rows,startval,endval,srow,erow))));
                    ## ------------------------------------------------------
                    timeindex=c(timeindex,seq(from=startval,to=endval,by=1));
                    aidx = seq(from=srow,to=erow,by=1);
                    aidxlen=length(aidx);
                    indices = c(indices,aidx);
                    count=c(count,1:length(aidx));
                    totalsweepcount = c(totalsweepcount,rep(aidxlen,aidxlen));

                    ## ------------------------------------------------------
                    avg_am_peak[i] = mean(x[,"Peak"][aidx],na.rm=T);
                    avg_am_peaks = c(avg_am_peaks,rep(avg_am_peak[i],aidxlen));
                    avg_am_tail[i] = mean(x[,"Tail"][aidx],na.rm=T);
                    avg_am_tails = c(avg_am_tails,rep(avg_am_tail[i],aidxlen));
                    ## avgpeakless: to calculate NARatio with less MgFreeEPSCs, only the last ones.
                    avg_am_peakless[i] = mean(x[,"Peak"][aidx[(aidxlen-min(c(aidxlen,less_sweeps)))+1:aidxlen]],na.rm=T);

                    avg_ar_peak[i] = mean(x[,"AreaPeak"][aidx],na.rm=T);
                    avg_ar_peaks = c(avg_ar_peaks,rep(avg_ar_peak[i],aidxlen));
                    avg_ar_tail[i] = mean(x[,"AreaTail"][aidx],na.rm=T);
                    avg_ar_tails = c(avg_ar_tails,rep(avg_ar_tail[i],aidxlen));
                    ## avgpeakless: to calculate NARatio with less MgFreeEPSCs, only the last ones.
                    avg_ar_peakless[i] = mean(x[,"AreaPeak"][aidx[(aidxlen-min(c(aidxlen,less_sweeps)))+1:aidxlen]],na.rm=T);

                    ## ------------------------------------------------------
                    avg_am_cvpeak[i] = (var(x[,"Peak"][aidx],na.rm=T)-var(x[,"Noise"][aidx],na.rm=T))/(mean(x[,"Peak"][aidx],na.rm=T)^2);
                    avg_am_cvpeaks = c(avg_am_cvpeaks,rep(avg_am_cvpeak[i],aidxlen));
                    avg_am_cvtail[i] = (var(x[,"Tail"][aidx],na.rm=T)-var(x[,"Noise"][aidx],na.rm=T))/(mean(x[,"Tail"][aidx],na.rm=T)^2);
                    avg_am_cvtails = c(avg_am_cvtails,rep(avg_am_cvtail[i],aidxlen));
                    
                    ## ------------------------------------------------------
                    avg_sr[i] = mean(x[,"SeriesRes"][aidx],na.rm=T);
                    avg_srs = c(avg_srs,rep(avg_sr[i],aidxlen));
                    avg_ir[i] = mean(x[,"InputRes"][aidx],na.rm=T);
                    avg_irs = c(avg_irs,rep(avg_ir[i],aidxlen));
                    avg_cap[i] = mean(x[,"MemCap"][aidx],na.rm=T);
                    avg_caps = c(avg_caps,rep(avg_cap[i],aidxlen));
                    avg_rt[i] = mean(x[,"RiseTime"][aidx],na.rm=T);
                    avg_rts = c(avg_rts,rep(avg_rt[i],aidxlen));
                    avg_slope[i] = mean(x[,"SlopeDirect"][aidx],na.rm=T);
                    avg_slopes = c(avg_slopes,rep(avg_slope[i],aidxlen));
                    avg_taupeak[i] = mean(x[,"TauPeak"][aidx],na.rm=T);
                    avg_taupeaks = c(avg_taupeaks,rep(avg_taupeak[i],aidxlen));
                    avg_tautail[i] = mean(x[,"TauTail"][aidx],na.rm=T);
                    avg_tautails = c(avg_tautails,rep(avg_tautail[i],aidxlen));
                    avg_taufull[i] = mean(x[,"TauFull"][aidx],na.rm=T);
                    avg_taufulls = c(avg_taufulls,rep(avg_taufull[i],aidxlen));
                    

                    ## --------------------------------------------------------
                    ## cat(c(dim(x[x[,"RecMode"]==arecmode[i],])[1],"\n",seq(from=srow,to=erow,by=1),"\n",seq(from=startval,to=endval,by=1),"\n"));
                }

                avg_am_ampa = avg_am_peak[1];
                avg_am_ampaless = avg_am_peakless[1];
                
                avg_ar_ampa = avg_ar_peak[1];
                avg_ar_ampaless = avg_ar_peakless[1];

                avg_am_cv_ampa = avg_am_cvpeak[1];
                
                if("MgFree"%in%unique(x[,"ExpType"])){
                    avg_am_nmda_neg = avg_am_peak[2];
                    avg_ar_nmda_neg = avg_ar_peak[2];
                    avg_am_nmdaless_neg = avg_am_peakless[2];
                    avg_ar_nmdaless_neg = avg_ar_peakless[2];
                    
                    nar_am_mgfree = abs(avg_am_nmda_neg/avg_am_ampa);
                    nar_am_mgfreeless = abs(avg_am_nmdaless_neg/avg_am_ampaless);                    
                    nar_ar_mgfree = abs(avg_ar_nmda_neg/avg_ar_ampa);
                    nar_ar_mgfreeless = abs(avg_ar_nmdaless_neg/avg_ar_ampaless);
                    
                    avg_am_cv_nmda_neg = avg_am_cvpeak[2];
                }
                if("MgFreeTest"%in%unique(x[,"ExpType"])){
                    avg_am_nmda_neg= avg_am_peak[3];
                    avg_ar_nmda_neg= avg_ar_peak[3];
                    
                    avg_am_nmda_pos= avg_am_tail[2];
                    avg_ar_nmda_pos= avg_ar_tail[2];
                    
                    avg_am_nmdaless_neg= avg_am_peakless[3];
                    avg_ar_nmdaless_neg= avg_ar_peakless[3];
                    
                    nar_am_mgfree = abs(avg_am_nmda_neg/avg_am_ampa);
                    nar_ar_mgfree = abs(avg_ar_nmda_neg/avg_ar_ampa);
                    
                    nar_am_mgfreeless = abs(avg_am_nmdaless_neg/avg_am_ampaless);                    
                    nar_ar_mgfreeless = abs(avg_ar_nmdaless_neg/avg_ar_ampaless);
                    
                    nar_am_depol = abs(avg_am_nmda_pos/avg_am_ampa);
                    nar_ar_depol = abs(avg_ar_nmda_pos/avg_ar_ampa);

                    avg_am_cv_nmda_neg = avg_am_cvpeak[3];
                    avg_am_cv_nmda_pos = avg_am_cvtail[2];
                }
                if("Depol"%in%unique(x[,"ExpType"])){
                    avg_am_nmda_pos= avg_am_tail[2];
                    avg_ar_nmda_pos= avg_ar_tail[2];
                    
                    nar_am_depol = abs(avg_am_nmda_pos/avg_am_ampa);
                    nar_ar_depol = abs(avg_ar_nmda_pos/avg_ar_ampa);

                    avg_am_prepost = avg_am_peak[4]/avg_am_ampa;
                    avg_am_prepostless = avg_am_peakless[4]/avg_am_ampaless

                    avg_am_cv_nmda_pos = avg_am_cvtail[2];
                }

                dfout=as.data.frame(cbind(x[indices,],
                    SweepCount = count,
                    TotalSweepCount = totalsweepcount,
                    TimeIndex = timeindex,
                    ## ---------------------
                    avgAMampa = rep(avg_am_ampa,length(indices)),
                    avgAMnmdaneg= rep(avg_am_nmda_neg,length(indices)),
                    avgAMnmdapos= rep(avg_am_nmda_pos,length(indices)),
                    avgAMampaless = rep(avg_am_ampaless,length(indices)),
                    avgAMnmdalessneg = rep(avg_am_nmdaless_neg,length(indices)),
                    avgAMpeak = avg_am_peaks,
                    avgAMtail = avg_am_tails,
                    narAMmgfree = rep(nar_am_mgfree,length(indices)),
                    narAMmgfreeless = rep(nar_am_mgfreeless,length(indices)),
                    narAMdepol = rep(nar_am_depol,length(indices)),
                    ## ---------------------
                    avgARampa = rep(avg_ar_ampa,length(indices)),
                    avgARnmdaneg = rep(avg_ar_nmda_neg,length(indices)),
                    avgARnmdapos = rep(avg_ar_nmda_pos,length(indices)),
                    avgARampaless = rep(avg_ar_ampaless,length(indices)),
                    avgARnmdalessneg = rep(avg_ar_nmdaless_neg,length(indices)),
                    avgARpeak = avg_ar_peaks,
                    avgARtail = avg_ar_tails,
                    narARmgfree = rep(nar_ar_mgfree,length(indices)),
                    narARmgfreeless = rep(nar_ar_mgfreeless,length(indices)),
                    narARdepol = rep(nar_ar_depol,length(indices)),
                    ## ---------------------
                    avgSeriesres = avg_srs/1E06,
                    avgInputres = avg_irs/1E06,
                    avgMemcap = avg_caps,
                    avgRisetime = avg_rts,
                    avgSlopedirect = avg_slopes,
                    avgTaupeak = avg_taupeaks,
                    avgTautail = avg_tautails,
                    avgTaufull = avg_taufulls,
                    ## --------------------
                    avgAMprepost = rep(avg_am_prepost,length(indices)),
                    avgAMprepostless = rep(avg_am_prepostless,length(indices)),
                    ## ---------------------
                    avgAMcvampa = avg_am_cv_ampa,
                    avgAMcvnmdaneg = avg_am_cv_nmda_neg,
                    avgAMcvnmdapos = avg_am_cv_nmda_pos,
                    ## --------------------
                    normAMpeak = (x[indices,"Peak"] / avg_am_peak[1])*100,
                    normARpeak = (x[indices,"AreaPeak"] / avg_ar_peak[1])*100,
                    normSeriesres = (x[indices,"SeriesRes"] / avg_sr[1])*100
                                          ));
                return(dfout);
            }

                     );
        return(EPSC);

    };
## =============================
#This function takes the standard values of CORT RIA assay along with sample value and outputs the CORT concentration
#Tue 12 Nov 2013 09:47:14 PM CET
#Given: Values of the standard and their respective cpm Bound/ cpm Free  (%B/F)
#CORT concentration is computed from %B/F of the sample
#smooth.spline(log(c(0.625,1.25,2.5,5.0,10,25,50,100))~c(96.25,93.1
#9,88.05,68.40,56.04,36.20,24.50,14.25),keep.data=T),x=98.70)$y)
#[1] 0.3456396
cortElisa = function(stdconc,stdcpmratio,samcpmratio)
   {

   }
