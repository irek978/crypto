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
                CoverDif<-str_dup("%", DifLength)
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
    
    Keycforig<-as.character(suborig)
    pastcforig<-paste0(c(OKey,SKey,SKey2,Keycforig,Keycfrsub),collapse="")
    
    
    startkeyorig<-gsub("1","À",pastcforig)
    Keycforig2<-gsub("2","Á",startkeyorig) 
    rm(startkeyorig)
    Keycforig3<-gsub("3","Â",Keycforig2)   
    rm(Keycforig2)
    Keycforig4<-gsub("4","Ã",Keycforig3)   
    rm(Keycforig3)
    Keycforig5<-gsub("5","Ä",Keycforig4)   
    rm(Keycforig4)
    Keycforig6<-gsub("6","Å",Keycforig5)   
    rm(Keycforig5)
    Keycforig7<-gsub("7","Ā",Keycforig6)   
    rm(Keycforig6)
    Keycforig8<-gsub("8","Æ",Keycforig7)   
    rm(Keycforig7)
    Keycforig9<-gsub("9","Ç",Keycforig8)   
    rm(Keycforig8)
    Keycforig0<-gsub("0","È",Keycforig9)
    rm(Keycforig9)
    KeycforigA<-gsub("A","É",Keycforig0)
    rm(Keycforig0)
    KeycforigB<-gsub("B","Ê",KeycforigA)
    rm(KeycforigA)
    KeycforigC<-gsub("C","Ë",KeycforigB)
    rm(KeycforigB)
    KeycforigD<-gsub("D","Ē",KeycforigC)
    rm(KeycforigC)
    KeycforigE<-gsub("E","Ì",KeycforigD)
    rm(KeycforigD)
    KeycforigF<-gsub("F","Í",KeycforigE)
    rm(KeycforigE)
    KeycforigG<-gsub("G","Î",KeycforigF)
    rm(KeycforigF)
    KeycforigH<-gsub("H","Ï",KeycforigG)
    rm(KeycforigG)
    KeycforigI<-gsub("I","Ī",KeycforigH)
    rm(KeycforigH)
    KeycforigJ<-gsub("J","Ð",KeycforigI)
    rm(KeycforigI)
    KeycforigK<-gsub("K","Ñ",KeycforigJ)
    rm(KeycforigJ)
    KeycforigL<-gsub("L","Ò",KeycforigK)
    rm(KeycforigK)
    KeycforigM<-gsub("M","Ó",KeycforigL)
    rm(KeycforigL)
    KeycforigN<-gsub("N","Ô",KeycforigM)
    rm(KeycforigM)
    KeycforigO<-gsub("O","Õ",KeycforigN)
    rm(KeycforigN)
    KeycforigP<-gsub("P","Ö",KeycforigO)
    rm(KeycforigO)
    KeycforigQ<-gsub("Q","Ø",KeycforigP)
    rm(KeycforigP)
    KeycforigR<-gsub("R","Ō",KeycforigQ)
    rm(KeycforigQ)
    KeycforigS<-gsub("S","Œ",KeycforigR)
    rm(KeycforigR)
    KeycforigT<-gsub("T","Š",KeycforigS)
    rm(KeycforigS)
    KeycforigU<-gsub("U","þ",KeycforigT)
    rm(KeycforigT)
    KeycforigV<-gsub("V","Ù",KeycforigU)
    rm(KeycforigU)
    KeycforigW<-gsub("W","Ú",KeycforigV)
    rm(KeycforigV)
    KeycforigY<-gsub("Y","Û",KeycforigW)
    rm(KeycforigW)
    KeycforigX<-gsub("X","Ü",KeycforigY)
    rm(KeycforigY)
    dfkeys<-gsub("Z","Ū",KeycforigX)
    rm(KeycforigX)
    
    
    write.csv(dfkeys, file="cryptokey.csv")
    write.csv(crypto, file = "crypto.csv")
    crypto  
}