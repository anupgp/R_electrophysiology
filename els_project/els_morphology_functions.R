## This function will combine the n<=N rows by adding them in an N x M  dataframe (matrix)
rowmerge <- function(mydf,n,idxcolname=""){
    ## Create empty dataframe for output
    newdf=data.frame();
    nrows=dim(mydf)[1];
    if(n>nrows){
        n=nrows;}
    col_ids = rep(TRUE,dim(mydf)[2]);
    idxcolpos=0;
    if(idxcolname==""){
        print("Running rowmerge without an index column\n");
    }else{
        ## Columns in the dataframe without the indxcolumn
        col_ids=!colnames(mydf)==idxcolname;
        ## Find the location of the idxcolumn in the dataframe
        idxcolpos=which(colnames(mydf)==idxcolname);
        ## Check if idxcolumn is present, else abort
        if(sum(!col_ids)==0){
            print("Given idxcolumn not present in the dataframe! Aborting rowmerge.\n");
            return(newdf);
        }
    }
    ## check if all the columns are numeric
    if(!all(sapply(mydf[,col_ids],FUN=is.numeric))==TRUE){
        print("!Error! Some columns in the dataframe are not numeric. Aborting rowmerge.\n");
        return(newdf);
    }
    beginrow=0;
    endrow=0;
    while(endrow<=nrows){
        beginrow=endrow+1;
        endrow=beginrow+n-1;
        if(endrow>nrows){
            endrow=nrows;
        }
        newdf=rbind(newdf,colSums(mydf[beginrow:endrow,col_ids]));
        if(endrow>=nrows){
            break;
        }
    }
    colnames(newdf)=colnames(mydf);
    return(newdf);
}
