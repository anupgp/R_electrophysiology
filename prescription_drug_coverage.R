## This is an R program to solve one of the challenge questions for fellowship program of the Data Incubator
## Time-stamp: <2017-02-04 11:31:13 anup>
rm(list=ls());
options(default.stringsAsFactors = FALSE);
source("/home/anup/goofy/projects/codes/els/R/loadLibraries.R");
options(contrasts=c("contr.sum","contr.poly"));
pdc2014 = read.csv('/home/anup/goofy/courses/dataincubator/challenge/PartD_Prescriber_PUF_NPI_14/PartD_Prescriber_PUF_NPI_14.txt',sep="\t",header=TRUE,stringsAsFactors=FALSE,row.names=NULL);
## Create a small subset of the data for checking the code by random sampling
randset = sample(1:dim(pdc)[1],1000,replace=FALSE);
pdcs = pdc[randset,];
## --------------------------------------------------------------
## Q2.1: What is the average number of beneficiaries per provider (> 10 beneficiaries and value not NA)
temp1 = pdcs[(pdcs[,"BENE_COUNT"] >10 & !is.na(pdcs[,"BENE_COUNT"])),"BENE_COUNT"]
avg_bene_prov = mean(temp1);
## --------------------------------------------------------------
## Q2.2: For each provider, estimate the length of the average prescription from TOTAL_DAY_SUPPLY and TOTAL_CLAIM_COUNT.
## What is the median, in days, of the distribution of this value across all providers
temp2 = ddply(pdcs,c("NPI"),function(x){
        avg_day_supply = (x[,"TOTAL_DAY_SUPPLY"]/x[,"TOTAL_CLAIM_COUNT"]);
        dfout = as.data.frame(cbind(NPI = unique(x[,"NPI"]),AVG_DAY_SUPPLY = avg_day_supply));
        return(dfout);
    })
med_avg_day_supply = median(temp2[,"AVG_DAY_SUPPLY"]);
## --------------------------------------------------------------
## Q2.3: What is the fraction of drug claims for each speciality that are for brand-name drugs
## Include only providers for whom the relevent information is not suppressed
## Consider only specilaities with at least 1000 total claims
## Compute standard deviation of the fractions

View(pdcs[(pdcs[,c("BRAND_SUPPRESS_FLAG")]) == "", c("TOTAL_CLAIM_COUNT","BRAND_CLAIM_COUNT","BRAND_SUPPRESS_FLAG","GENERIC_CLAIM_COUNT","GENERIC_SUPPRESS_FLAG")])

temp3 = ddply(pdcs,c("SPECIALTY_DESCRIPTION"),function(x){
    x2 = x[x[,"BRAND_SUPPRESS_FLAG"] == "",];
    sum_total_claim_count = sum(x2[,"TOTAL_CLAIM_COUNT"]);
    sum_brand_claim_count = sum(x2[,"BRAND_CLAIM_COUNT"]);
    frac_brand_claim_count = sum_brand_claim_count / sum_total_claim_count;
    if (sum_total_claim_count >= 1000){
        dfout = as.data.frame(cbind(SPECIALTY_DESCRIPTION=unique(x2[,"SPECIALTY_DESCRIPTION"]),
            BRAND_CLAIM_FRAC = frac_brand_claim_count,
            BRAND_CLAIM_COUNT = sum_brand_claim_count,
            TOTAL_CLAIM_COUNT = sum_total_claim_count)
                              )
    return(dfout);}
    
})
  
sdev_brand_claim_frac = sd(temp2[,"BRAND_CLAIM_FRAC"]);
## ---------------------------------------------------------------
## Q2.4: Find the ratio of beneficiaries with opioid prescriptions to beneficiaries with antibiotics prescriptions in each state
## What is the difference between the largest and smallest ratios?

View(pdcs[(pdcs[,"NPPES_PROVIDER_STATE"] == "CA" & !is.na(pdcs[,"OPIOID_BENE_COUNT"]) & !is.na(pdcs[,"ANTIBIOTIC_BENE_COUNT"])),c("NPPES_PROVIDER_STATE","OPIOID_BENE_COUNT","ANTIBIOTIC_BENE_COUNT")])

temp4 = ddply(pdcs,c("NPPES_PROVIDER_STATE"),function(x){
    x2 = x[(!is.na(x[,"OPIOID_BENE_COUNT"]) & !is.na(x[,"ANTIBIOTIC_BENE_COUNT"])),];
    sum_opioid_bene_count = as.numeric(sum(x2[,"OPIOID_BENE_COUNT"],na.rm=TRUE));
    sum_anti_bene_count = as.numeric(sum(x2[,"ANTIBIOTIC_BENE_COUNT"],na.rm=TRUE));
    if (sum_anti_bene_count != 0){
    opioid_anti_bene_ratio = sum_opioid_bene_count/sum_anti_bene_count;
    dfout = as.data.frame(cbind(NPPES_PROVIDER_STATE=unique(x2[,"NPPES_PROVIDER_STATE"]),
        OPIOID_BENE_SUM = sum_opioid_bene_count,
        ANTIBIOTIC_BENE_SUM = sum_anti_bene_count,
        OPIOID_ANTIBIOTIC_BENE_RATIO = as.numeric(opioid_anti_bene_ratio)))
    return(dfout);}
})

diff_largest_small_ratio = as.numeric(max(levels(temp4[,"OPIOID_ANTIBIOTIC_BENE_RATIO"]))) - as.numeric(min(levels(temp4[,"OPIOID_ANTIBIOTIC_BENE_RATIO"])))
## ----------------------------------------------------------------
## Q2.5: Fraction of claims for beneficiaries age 65 and older
## Fraction of claims for beneficiaries with low-income subsidy
## what is the Pearson correlation coefficient between these values
temp5 = ddply(pdcs,c("NPI"),function(x){
    x2 = x[x[,"GE65_SUPPRESS_FLAG"] == "",];
    total_claim_count = x2[,"TOTAL_CLAIM_COUNT"];
    ge65_claim_count = x2[,"TOTAL_CLAIM_COUNT_GE65"];
    ge65_claim_frac = ge65_claim_count/total_claim_count;
    x3 = x[x[,"LIS_SUPPRESS_FLAG"] == "",];
    lis_claim_count = x3[,"LIS_CLAIM_COUNT"];
    lis_claim_frac = lis_claim_count/total_claim_count;
    if(x[,"GE65_SUPPRESS_FLAG"] == "" & x[,"LIS_SUPPRESS_FLAG"] == ""){
        dfout = as.data.frame(cbind(NPI = unique(x[,"NPI"]),
            GE65_CLAIM_FRAC = ge65_claim_frac,
            LIS_CLAIM_FRAC = lis_claim_frac));
        return(dfout);}
});
cor1 = cor.test(x=temp5[,"GE65_CLAIM_FRAC"],y=temp5[,"LIS_CLAIM_FRAC"],method="pearson")
cor2 = cor(x=temp5[,"GE65_CLAIM_FRAC"],y=temp5[,"LIS_CLAIM_FRAC"],method="pearson")
## --------------------------------------------------------------------
## Q2.6: Find states with high supply of opioids, conditioned on speciality
## Average length of a opioid prescription for each provider
## For each state and speciality pair, with at least 100 providers, calculate average across all the providers
## Find the ratio of the above value to an equivalent quatitiy calculated from providers in each speciality across all states
## Find the largest of this ratio
## obc=by(pdcs,pdcs[,"NPPES_PROVIDER_STATE"],function(x){return(sum(x[,"OPIOID_BENE_COUNT"],na.rm=TRUE))},simplify = TRUE)
temp6 = ddply(pdcs,c("SPECIALTY_DESCRIPTION"),function(x){
    obc=ddply(x,c("NPPES_PROVIDER_STATE"),function(y){
        numproviders = length(y[,"NPI"]);
        ## print(class(numproviders));
        ## print(numproviders)
        dfout1 = as.data.frame(cbind(NPPES_PROVIDER_STATE=unique(y[,"NPPES_PROVIDER_STATE"]),
            SUM_OPIOID_BENE_COUNT = sum(y[,"OPIOID_BENE_COUNT"],na.rm=TRUE),
            NUM_PROVIDERS = as.numeric(as.character(numproviders))),
            stringsAsFactors = FALSE);
        return (dfout1);});
    print(obc);

    obc[,"SUM_OPIOID_BENE_COUNT"] = as.numeric(obc[,"SUM_OPIOID_BENE_COUNT"]);
    print(c(dim(obc),max(obc[,"SUM_OPIOID_BENE_COUNT"])));
    obcmaxstate = obc[obc[,"SUM_OPIOID_BENE_COUNT"]==max(obc[,"SUM_OPIOID_BENE_COUNT"]),"NPPES_PROVIDER_STATE"];
    ## obcmaxstate = paste0(" ",unlist(obcmaxstate),collapse = "");
    print(obcmaxstate);
    if(length(obcmaxstate) == 274){
        stop("");
    }
    avglenobc = ddply(x,c("NPI"),function(y){
        dfout2 = as.data.frame(cbind(NPI2=unique(y[,"NPI"]),
            AVG_OPIOID_DAY_SUPPLY = (y[,"TOTAL_DAY_SUPPLY"]/y[,"TOTAL_CLAIM_COUNT"])),stringsAsFactors = FALSE);
        return (dfout2);},.parallel = FALSE)
    print(avglenobc);
    
    dfout3 = as.data.frame(cbind(SPECIALTY_DESCRIPTION2 = rep(unique(x[,"SPECIALTY_DESCRIPTION"]),length(x[,"SPECIALTY_DESCRIPTION"])),
        OPIOID_MAX_STATE=obcmaxstate,
        ## OPIOID_MAX_STATE=obcmaxstate,
        NUM_PROVIDERS = obc[,"NUM_PROVIDERS"],
        avglenobc),stringsAsFactors = FALSE);
    ## print(dfout3);
    return(dfout3);})

is.factor(temp6[,"NUM_PROVIDERS"])

## merge data frames
temp7 = merge(pdcs,temp6[,c("NPI","OPIOID_MAX_STATE","AVG_OPIOID_DAY_SUPPLY","NUM_PROVIDERS")],by=c("NPI"));

temp8 = ddply(temp7,c("NPPES_PROVIDER_STATE","SPECIALTY_DESCRIPTION"),function(x){
    numproviders = as.numeric(as.character(x[,"NUM_PROVIDERS"][1]));
    avg_avg_opioid_day_supply = mean(x[,"AVG_OPIOID_DAY_SUPPLY"]);
    if(numproviders >= 100){
        print(numproviders);
        dfout = as.data.frame(cbind(x,AVG_OPIOID_DAY_SUPPLY_PROV=rep(avg_avg_opioid_day_supply,length(x[,"NPI"]))),stringsAsFactors = FALSE)
        return(dfout);
    }
})
        
    
