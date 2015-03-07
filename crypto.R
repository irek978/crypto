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
    UCVal<-subset(unique(CVal),unique(CVal)!="")
    
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
            if(str_length(dfcolumnname[r])==CLength) {
                rplcn<-substr(str_replace_all(dfcolumnname[r],OKey[n],SKey[n]),n,n)
                sorig<-substr(str_replace_all(dfcolumnname[r],OKey[n],OKey[n]),n,n) 
            }
            if(str_length(dfcolumnname[r])!=CLength) {
                DifLength<-CLength-str_length(dfcolumnname[r])
                CoverDif<-str_dup("§", DifLength)
                rplcn<-substr(paste(c(dfcolumnname[r],CoverDif),collapse=""),n,n)   
                sorig<-substr(paste(c(dfcolumnname[r],CoverDif),collapse=""),n,n)   
            }
            subrplcnx<-rbind(c(subrplcnx,rplcn))
            suborigx<-rbind(c(suborigx,sorig))       
        }
        subrplcn<-c(subrplcn,subrplcnx)
        suborig<-c(suborig,suborigx)
    }
    
    cfrsub<-cbind(subrplcn,pos=1:NRow)
    cforig<-cbind(suborig,pos=1:NRow)
    cfrsubstr<-as.character(subrplcn)
    cforigstr<-as.character(suborig)
    
    matchrow<-cbind(rbind(cforig),rbind(cfrsub))
    
    checkmatch<-as.character(matchrow[,1])!=as.character(matchrow[,3])
    matchrowl<-cbind(rbind(matchrow),cbind(checkmatch))
    dfmatch<-data.frame(matchrowl)
    dimmatch<-nrow(dfmatch) 
    SKey2<-cbind(sample(AllVal,(dimmatch),replace = TRUE))
    dfmatchs<-cbind(cbind(dfmatch),rbind(SKey2))
    
    putdef<-data.frame()
    putkeyorig<-list()
    for(r in 1:dimmatch) {
        putdata<-data.frame()
        sborigx<-list()
        if (dfmatch[r,5]==TRUE & dfmatchs[r,3]!="") {
            put<-as.character(dfmatchs[r,3])
            sborigxn<-as.character(dfmatchs[r,1])
            putdata<-rbind(c(putdata,put))
            sborigx<-c(sborigx,sborigxn)
        }
        if (dfmatch[r,5]==FALSE | dfmatchs[r,3]=="") {
            put<-as.character(dfmatchs[r,6])
            sborigxn<-as.character("ø")
            putdata<-rbind(c(putdata,put))
            sborigx<-c(sborigx,sborigxn)
        }
        putdef<-c(putdata,putdef)
        putkeyorig<-c(putkeyorig,sborigx)
    }
    
    colsplit<-split(putdef, 1:CLength)
    
    NCol<-length(colsplit[[CLength]])
    crypto <- character()
    for(i in 1:NCol) {
        word <- character()    
        for(j in 1:CLength) {
            word <- paste(word, colsplit[[j]][[i]], sep = "")
            j=j+1
        }
        crypto <- c(crypto, word)
        i=i+1
    }
    
    Keycfrsub<-as.character(subrplcn)
    Keycforig<-as.character(putkeyorig)
    
    cryptokey<-paste(c(OKey,SKey,SKey2,Keycforig,Keycfrsub),collapse="")
    
    write.csv(cryptokey, file="cryptokey.csv")
    write.csv(crypto, file = "crypto.csv")
    crypto
}