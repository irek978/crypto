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
    
    
    dedfkeys<-as.character(cryptokeys[1,2])
    
    
    Ldedfkeys<-str_length(dedfkeys)
    submatch<-(Ldedfkeys-CLength-CLength)/3
    
    OKeyxn<-vector()
    for (l in 1:CLength) {
        OKeyx<-substr(substr(dedfkeys,1,CLength),l,l)
        OKeyxn<-c(OKeyxn,OKeyx)
    }
    
    SKeyxn<-vector()
    for (l in 1:CLength) {
        SKeyx<-substr(substr(dedfkeys,as.numeric(CLength+1),as.numeric(CLength+CLength)),l,l)
        SKeyxn<-c(SKeyxn,SKeyx)
    }
    
    SKeyxn2<-vector()
    for (d in 1:submatch) {
        SKeyx2<-substr(substr(dedfkeys,as.numeric(CLength+CLength+1),as.numeric(submatch+CLength+CLength)),d,d)
        SKeyxn2<-c(SKeyxn2,SKeyx2)
    }
    
    LCrypto<-list()
    for (r in 1:NRow) {
        LCrypton<-list()
        for (n in 1:CLength) {
            LCryptoxn<-substr(crypto$x[r],n,n)
            LCrypton<-c(LCrypton,LCryptoxn)
        }
        LCrypto<-c(LCrypto,LCrypton)
    }
    
    CFKeyorigxn<-list()
    for (d in 1:submatch) {
        SKorig<-substr(substr(dedfkeys,as.numeric(CLength+CLength+submatch+1),as.numeric(CLength+CLength+submatch+submatch)),d,d)
        CFKeyorigxn<-c(CFKeyorigxn,SKorig)
    }
    
    CFKeycfrsub<-list()
    for (d in 1:submatch) {
        SKsub<-substr(substr(dedfkeys,as.numeric(CLength+CLength+submatch+submatch+1),as.numeric(Ldedfkeys)),d,d)
        CFKeycfrsub<-c(CFKeycfrsub,SKsub)
    }
    
    deccrypto<-cbind(LCrypto,pos=1:NRow) 
    decfrsub<-cbind(CFKeycfrsub,pos=1:NRow)
    decforig<-cbind(CFKeyorigxn,pos=1:NRow)
    
    dematchrow<-cbind(rbind(deccrypto), rbind(decfrsub))
    decheckmatch<-as.character(dematchrow[,1])!=as.character(dematchrow[,3])
    dematchrowl<-cbind(rbind(dematchrow),cbind(decheckmatch),cbind(SKeyxn2), cbind(decforig))
    dedfmatch<-data.frame(dematchrowl)
    dedimmatch<-nrow(dedfmatch) 
    
    deputdef<-data.frame()
    for(r in 1:dedimmatch) {
        deputdata<-data.frame()    
        if (dedfmatch[r,5]==FALSE & dedfmatch[r,3]!="§" & dedfmatch[r,7]=="ø") {
            deput<-as.character(dedfmatch[r,3])
            deputdata<-rbind(c(deputdata,deput))
        }
        if (dedfmatch[r,5]==FALSE & dedfmatch[r,3]!="§" & dedfmatch[r,7]!="ø") {
            deput<-as.character(dedfmatch[r,7])
            deputdata<-rbind(c(deputdata,deput))
        }
        if (dedfmatch[r,3]=="§" & dedfmatch[r,7]=="ø") {
            deput<-as.character(dedfmatch[r,3])
            deputdata<-rbind(c(deputdata,deput))
        }
        if (dedfmatch[r,5]==TRUE & dedfmatch[r,7]!="ø" & dedfmatch[r,3]!="§") {
            deput<-as.character(dedfmatch[r,7])
            deputdata<-rbind(c(deputdata,deput))
        }
        if (dedfmatch[r,5]==TRUE & dedfmatch[r,7]=="ø" & dedfmatch[r,3]!="§") {
            deput<-as.character(dedfmatch[r,3])
            deputdata<-rbind(c(deputdata,deput))
        }
        deputdef<-cbind(c(deputdef,deputdata))
    }
    
    decolsplit<-split(deputdef, 1:CLength)
    
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
    
    decrypto<-gsub("§","", decryptox)
    decrypto
    
}
