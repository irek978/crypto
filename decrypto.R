decrypto <-function(cryptopath) {
    setwd(cryptopath)
    cryptokeys<-read.csv("cryptokey.csv")
    crypto<-read.csv("crypto.csv")
    
    options(warn=-1)
    
    if (!require('stringr')) {
        stop('The package stringr is required. Please install it and try again')
    }
    
    if(class(crypto)!="data.frame") {
        stop('Crypto works only with data.frame. Please check your input data and try again')
    }
    
    options(warn=-0)
    
    CLength<-unique(max(str_length(crypto$x)))
    NRow<-length(crypto$x)
    dfkeysx<-as.character(cryptokeys[1,2])
    
    startkeyorig<-gsub("À","1",dfkeysx)
    Keycforig2<-gsub("Á","2",startkeyorig) 
    rm(startkeyorig)
    Keycforig3<-gsub("Â","3",Keycforig2)   
    rm(Keycforig2)
    Keycforig4<-gsub("Ã","4",Keycforig3)   
    rm(Keycforig3)
    Keycforig5<-gsub("Ä","5",Keycforig4)   
    rm(Keycforig4)
    Keycforig6<-gsub("Å","6",Keycforig5)   
    rm(Keycforig5)
    Keycforig7<-gsub("Ā","7",Keycforig6)   
    rm(Keycforig6)
    Keycforig8<-gsub("Æ","8",Keycforig7)   
    rm(Keycforig7)
    Keycforig9<-gsub("Ç","9",Keycforig8)   
    rm(Keycforig8)
    Keycforig0<-gsub("È","0",Keycforig9)
    rm(Keycforig9)
    KeycforigA<-gsub("É","A",Keycforig0)
    rm(Keycforig0)
    KeycforigB<-gsub("Ê","B",KeycforigA)
    rm(KeycforigA)
    KeycforigC<-gsub("Ë","C",KeycforigB)
    rm(KeycforigB)
    KeycforigD<-gsub("Ē","D",KeycforigC)
    rm(KeycforigC)
    KeycforigE<-gsub("Ì","E",KeycforigD)
    rm(KeycforigD)
    KeycforigF<-gsub("Í","F",KeycforigE)
    rm(KeycforigE)
    KeycforigG<-gsub("Î","G",KeycforigF)
    rm(KeycforigF)
    KeycforigH<-gsub("Ï","H",KeycforigG)
    rm(KeycforigG)
    KeycforigI<-gsub("Ī","I",KeycforigH)
    rm(KeycforigH)
    KeycforigJ<-gsub("Ð","J",KeycforigI)
    rm(KeycforigI)
    KeycforigK<-gsub("Ñ","K",KeycforigJ)
    rm(KeycforigJ)
    KeycforigL<-gsub("Ò","L",KeycforigK)
    rm(KeycforigK)
    KeycforigM<-gsub("Ó","M",KeycforigL)
    rm(KeycforigL)
    KeycforigN<-gsub("Ô","N",KeycforigM)
    rm(KeycforigM)
    KeycforigO<-gsub("Õ","O",KeycforigN)
    rm(KeycforigN)
    KeycforigP<-gsub("Ö","P",KeycforigO)
    rm(KeycforigO)
    KeycforigQ<-gsub("Ø","Q",KeycforigP)
    rm(KeycforigP)
    KeycforigR<-gsub("Ō","R",KeycforigQ)
    rm(KeycforigQ)
    KeycforigS<-gsub("Œ","S",KeycforigR)
    rm(KeycforigR)
    KeycforigT<-gsub("Š","T",KeycforigS)
    rm(KeycforigS)
    KeycforigU<-gsub("þ","U",KeycforigT)
    rm(KeycforigT)
    KeycforigV<-gsub("Ù","V",KeycforigU)
    rm(KeycforigU)
    KeycforigW<-gsub("Ú","W",KeycforigV)
    rm(KeycforigV)
    KeycforigY<-gsub("Û","Y",KeycforigW)
    rm(KeycforigW)
    KeycforigX<-gsub("Ü","X",KeycforigY)
    rm(KeycforigY)
    dfkeys<-gsub("Ū","Z",KeycforigX)
    rm(KeycforigX)
    
    Ldfkeys<-str_length(dfkeys)
    submatch<-(Ldfkeys-CLength-CLength)/3
    
    OKeyxn<-vector()
    for (l in 1:CLength) {
        OKeyx<-substr(substr(dfkeys,1,CLength),l,l)
        OKeyxn<-c(OKeyxn,OKeyx)
    }
    
    SKeyxn<-vector()
    for (l in 1:CLength) {
        SKeyx<-substr(substr(dfkeys,as.numeric(CLength+1),as.numeric(CLength+CLength)),l,l)
        SKeyxn<-c(SKeyxn,SKeyx)
    }
    
    SKeyxn2<-vector()
    for (d in 1:submatch) {
        SKeyx2<-substr(substr(dfkeys,as.numeric(CLength+CLength+1),as.numeric(submatch)),d,d)
        SKeyxn2<-c(SKeyxn2,SKeyx2)
    }
    
    CFKeyorigxn<-list()
    for (d in 1:submatch) {
        SKorig<-substr(substr(dfkeys,as.numeric(CLength+CLength+submatch+1),as.numeric(CLength+CLength+submatch+submatch)),d,d)
        CFKeyorigxn<-c(CFKeyorigxn,SKorig)
    }
    
    decolsplit<-split(CFKeyorigxn, 1:CLength)
    
    NColde<-length(decolsplit[[CLength]])
    
    decryptox <- character()
    for(i in 1:NColde) {
        word <- character()    
        for(j in 1:CLength) {
            word <- paste(word,decolsplit[[j]][[i]], sep = "")
            j=j+1
        }
        decryptox <- c(decryptox, word)
        i=i+1
    }
    
    decrypto<-gsub("%","", decryptox)
    decrypto
    
}
