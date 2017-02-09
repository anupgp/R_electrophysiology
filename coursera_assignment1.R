pollutantmean <- function(directory,pollutant,id = 1:332){
    dc = c();
    count = 0;
    for (i in id){
        count = count + 1;
        if(i < 10){
           fname = paste(directory,"/","00",i,".csv",sep="");
       }
        else if (i < 100){
            fname = paste(directory,"/","0",i,".csv",sep="");
        }
        else{
         fname = paste(directory,"/",i,".csv",sep="");
     }
        d = read.csv(fname);
        dc = c(dc,d[!is.na(d[pollutant]),pollutant]);
    }
    return(mean(dc));
}

## =======================================================

complete <- function(directory,id = 1:332){
    full = data.frame(matrix(NA,length(id),2));
    colnames(full) <- c("id","nobs");
    count = 0;
    for (i in id){
        count = count + 1;
        if(i < 10){
           fname = paste(directory,"/","00",i,".csv",sep="");
       }
        else if (i < 100){
            fname = paste(directory,"/","0",i,".csv",sep="");
        }
        else{
         fname = paste(directory,"/",i,".csv",sep="");
     }
        d = read.csv(fname);
        full[count,"id"] = i;
        full[count,"nobs"] = length(which(complete.cases(d)));
    }
    return(full);
}

## ================================================================

corr <- function(directory,threshold = 0){
    comp_df = complete(directory,1:332);
    thres_ids = comp_df[comp_df["nobs"] > threshold,"id"];
    corrs = c();
    count = 0;
    for (i in thres_ids){
        count = count + 1;
        if(i < 10){
            fname = paste(directory,"/","00",i,".csv",sep="");
        }
        else if (i < 100){
            fname = paste(directory,"/","0",i,".csv",sep="");
        }
        else{
            fname = paste(directory,"/",i,".csv",sep="");
        }
        d = read.csv(fname);
        corrs[count] = cor(d[which(complete.cases(d)),"sulfate"],d[which(complete.cases(d)),"nitrate"]);
    }
    return(corrs);
}
