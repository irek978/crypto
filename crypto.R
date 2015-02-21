crypto <-function(df,dfcolumnname) {
    options(warn=-1)
    
    if (!require('stringr')) {
        stop('The package stringr is required. Please install it and try again')
    }
    
    if(class(df)!="data.frame") {
        stop('Crypto works only with data.frame. Please check your input data and try again')
    }
    
    options(warn=-0)
    
    CLength<-unique(max(str_length(dfcolumnname)))
    NRow<-length(dfcolumnname)
    CVal<-vector()
    n=0
    for (n in 1:CLength) {
        sub<-substr(dfcolumnname,n,n)  
        CVal<-c(CVal,sub)
        n=n+1
    }
    UCVal<-unique(CVal)
    
    RandomVal<-toupper(c(0:9, letters))
    
    AllVal<-toupper(c(0:9, letters))
    NAllVal<-length(AllVal)
    SKey<-sample(AllVal,(CLength),replace = TRUE)
    OKey<-sample(UCVal,(CLength),replace=TRUE)
    
    subrplcn<-data.frame()
    suborig<-data.frame()
    for (r in 1:NRow) {
        subrplcnx<-data.frame()
        suborigx<-data.frame()
        for(n in 1: CLength) {
            rplcn<-substr(str_replace_all(dfcolumnname[r],OKey[n],SKey[n]),n,n)
            subrplcnx<-rbind(c(subrplcnx,rplcn))
            sorig<-substr(str_replace_all(dfcolumnname[r],OKey[n],OKey[n]),n,n)
            suborigx<-rbind(c(suborigx,sorig))
        }
        subrplcn<-c(subrplcn,subrplcnx)
        suborig<-c(suborig,suborigx)
    }
    
    cfrsub<-cbind(subrplcn,pos=1:NRow)
    cforig<-cbind(suborig,pos=1:NRow)
    cfrsubstr<-as.character(subrplcn)
    
    matchrow<-cbind(rbind(cforig), rbind(cfrsub))
    
    checkmatch<-as.character(matchrow[,1])!=as.character(matchrow[,3])
    matchrowl<-cbind(rbind(matchrow),cbind(checkmatch))
    dfmatch<-data.frame(matchrowl)
    dimmatch<-nrow(dfmatch) 
    SKey2<-sample(AllVal,(dimmatch),replace = TRUE)
    
    putdef<-data.frame()
    for(r in 1:dimmatch) {
        putdata<-data.frame()    
        if (dfmatch[r,5]==TRUE & dfmatch[r,5]!="") {
            put<-as.character(dfmatch[r,3])
            putdata<-rbind(c(putdata,put))
        }
        if (dfmatch[r,5]==FALSE |dfmatch[r,5]=="") {
            put<-str_replace_all(as.character(dfmatch[r,3]),as.character(dfmatch[r,3]),SKey2[as.numeric(dfmatch[r,2])])        
            putdata<-rbind(c(putdata,put))
        }
        putdef<-c(putdata,putdef)
    }
    
    colsplit<-split(putdef, 1:CLength)
    
    NCol<-length(colsplit[[CLength]])
    r_crypto <- character()
    for(i in 1:NCol) {
        word <- character()    
        for(j in 1:CLength) {
            word <- paste(word, colsplit[[j]][[i]], sep = "")
            j=j+1
        }
        r_crypto <- c(r_crypto, word)
        i=i+1
    }
    
    dfkeys<-cbind(OKey,SKey,SKey2)
    
    write.csv(dfkeys, file="resultkeys.csv")
    write.csv(r_crypto, file = "crypto.csv")
    r_crypto  
}
