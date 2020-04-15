#Functions to call such as cookies disect, pvalues etc
#library(httr) #apt-get install libcurl4-openssl-dev

#####get overlap#####
#send persona and crawl list, this function will match left (of right)
# and then count overlap percentage per website
getOverlap <- function(personaDf,crawl,percent=T){
  
  #crawl cookies, left and right
  indL <- crawl$visit_id%in%lhVisitID()
  crawlHostsLeft <- gsub("[.]","",crawl$raw_host[indL])
  indR <- !crawl$visit_id%in%lhVisitID()
  crawlHostsRight <- gsub("[.]","",crawl$raw_host[indR])
  #persona cookies
  personaHosts <-gsub("[.]","",personaDf[,3]) 
  
  crawlDf <- crawl[indL,]
  domainsOverlap <- crawlDf[crawlHostsLeft%in%personaHosts,]
  left <- (sortedTable(c(domainsOverlap$visit_id,unique(crawlDf$visit_id)))-1)/sortedTable(crawlDf$visit_id)
  crawlDf <- crawl[indR,]
  domainsOverlap <- crawlDf[crawlHostsRight%in%personaHosts,]
  right <- (sortedTable(c(domainsOverlap$visit_id,unique(crawlDf$visit_id)))-1)/sortedTable(crawlDf$visit_id)
  #plotCDF(cdf = c("L"=list(left),"R"=list(right)),legPos = c(0.5,0.5))
  c(list(left),list(right))
}

####Utility Functions#####
getHeaderCook <- function(htReq){
  header <- htReq$headers
  ind <- grep("Cookie",header)
  cookie <- header[ind]
  cookie <- gsub('.*\"Cookie\",\"',"",cookie)
  cookie <- gsub('[^.*]\".*',"",cookie)
  cookHeader <- rep(NA,length(header))
  cookHeader[ind] <- cookie
  cookHeader
}

#####Export to csv#####
#update later, if exists 
exportSQLCSV <- function(df,file,edit=F,sep=" "){
  if(file.exists(file)){
    if(edit==T){
      cat("Editing")
    }
  }else{
    cat("writing")
    #write.csv(df,file,sep = sep)
    write.table(df,file,sep = sep,quote = F,row.names = F)
  }
}

####### T and ks test#####

#check list ditributions p-values
checkPvalue <- function(crawlRun,test="ks",scaleName="Scale"){
  #t-test, [[1]]=t, 2=df, 3=p
  #ks-test 2=p
  tRes <- matrix(NA,nrow = length(crawlRun),ncol = length(crawlRun))
  crawlRun<- crawlRun[order(names(crawlRun))]
  for(i in 1:length(crawlRun)){
    for(j in (i):length(crawlRun)){
      if(test=="ks"){
        #cat("using KS, 2P")
        tRes[i,j] <- 0
        p <- ks.test(crawlRun[[i]],crawlRun[[j]],alternative = "g")[[2]]
        if(p<=0.01){
            tRes[i,j] <- ks.test(crawlRun[[i]],crawlRun[[j]],alternative = "g")[[1]]
            #cat(p," ")
        }
      }
      else if(test=="t"){
        p <- t.test(crawlRun[[i]],crawlRun[[j]])[[3]]
        if(p<=0.01){
          tRes[i,j] <- t.test(crawlRun[[i]],crawlRun[[j]])[[1]]
        }
      } else if(test=="cor"){
        tRes[i,j] <- cor(crawlRun[[i]],crawlRun[[j]])
      }
    }
  }
  
  rownames(tRes) <- colnames(tRes) <- names(crawlRun)
  #tRes[lower.tri(tRes)] <- NA
  diag(tRes) <- NA
  #heatmap(tRes)
  #tRes <- round(tRes,2)
  melted_cormat <- melt(tRes, na.rm = TRUE)
  #print(melted_cormat)
  
  # melted_cormat$value[melted_cormat$value<0.01] <- 0
  # melted_cormat$value[melted_cormat$value>=0.01&&melted_cormat$value<0.2] <- 0.5
  # melted_cormat$value[melted_cormat$value>=0.2] <- 1
  #melted_cormat <- melted_cormat[order(melted_cormat[,1]),]
  cat("using colors- red, yellow, blue")
  print(plotHeatGGP(melted_cormat,scale = scaleName,legPos = "right"))
  #print(melted_cormat[melted_cormat$value<0.01,])
}

#####cookies type######

cookieType <- function(jsCookie,pCookie,ind){
  
  visitId <- c(jsCookie$visit_id,pCookie$visit_id)
  jsHost <- c(jsCookie$raw_host,pCookie$baseDomain)[visitId%in%which(lh$political_category=="right")]
  len <- length(jsHost)
  
  rtHost <- typeCookie(jsHost)
  
  hostLh <- data.frame(id=names(rtHost),value=rtHost/len*100,
                       var="Right",ind=ind,stringsAsFactors = F)
  jsHost <- c(jsCookie$raw_host,pCookie$baseDomain)[visitId%in%which(lh$political_category=="left")]
  len <- length(jsHost)
  lfHost <- typeCookie(jsHost)
  hostLh <- rbind(hostLh,data.frame(id=names(lfHost),value=lfHost/len*100,
                                    var="Left",ind=ind,stringsAsFactors = F))
  #check rest for 1sty party
  hostLh
}

#using https://github.com/disconnectme/disconnect-tracking-protection/blob/master/services.json
#by categories- https://disconnect.me/trackerprotection#categories-of-trackers
#returning cookies names matching
typeCookie <- function(domainCheck){
  #just import plain string
  host <- domainCheck
  #remove X from excel
  host <-gsub("X","",host) 
  #placeholder for adding type
  domainCheckType <-c()  
  cat("Len",length(host))
  
  disconnectDomains <- getfiles(name = "disc")  
  
  uqType <- unique(disconnectDomains$type)
  x <- uqType[1]
  #direct match host and urls first
  exactHost <- lapply(uqType, function(x){
    val <- disconnectDomains[which(disconnectDomains$type==x),]
    ind <- match(host,val$url)
    df <- cbind(host=val$url[na.omit(ind)],val[na.omit(ind),])
    host <<- host[is.na(match(host,val$url))]
    #cat(x,nrow(df),"\n")
    df
  })
  
  exactHost <- do.call(rbind,exactHost)
  
  cat("Found direct hosts",length(domainCheck)-length(host),"\n")
  
  s <- uqType[2]
  
  substringHost <- lapply(uqType, function(s){
    #cat(s)
    x <- disconnectDomains[which(disconnectDomains$type==s),]
    x <- x[!(nchar(x$url)<=1),]
    #for each domain of category find in data
    #since exact matched early, this will be [dot]url
    #removing dot as https://spectaor.org
    #carefull, add [dot] to keep care of names matching
    i <- 1576
    substr <- c()
    for(i in 1:nrow(x)){
      str <- paste0("",x$url[i])
      ind <- grep(str,host,ignore.case = T)
      if(length(ind)>0){
        #cat(i,str,host[ind],"\n")
        #30 or 16 for FP and Crpto
        # if(length(x)==30|length(x)==16){
        #   cat(x[i],length(ind),"\n")
        # }
        substr <- rbind(substr,cbind(host=host[ind],x[i,]))
        host <<- host[-ind]
        }
    }
    substr
  })
  
  #267 with one.; 239 with nchar>1
  substringHost <- do.call(rbind,substringHost)
  #sortedTable(substringHost$host)[1:10]
  cat("Found subdomain hosts",length(domainCheck)-length(host),"\n")
  
  #update host with top domains using last 2
  topD <- paste0(fileExplodeName(host,using = "[.]",param =2,last = T),".",
    fileExplodeName(host,using = "[.]",param =1,last = T))
  host <- cbind(host,type="Others",topD,topD,host)
  colnames(host) <- colnames(substringHost)
  
  allDomainType <- rbind(exactHost,substringHost,host)
  cat("Returning ")
  print(sortedTable(allDomainType$type))
  allDomainType
}

###Disconnect Domains####
#use unlist for melted data, exported csv. use this for updating
disconnectDomainsList <- function(unlist=F,figCrpto=F){
  #disconnect2 <- fromJSON("./csv_read/services.json",simplifyVector = F,simplifyDataFrame = F,simplifyMatrix = F)
  disconnect <- fromJSON(XXX,simplifyVector = F,simplifyDataFrame = F,simplifyMatrix = F)
  fp <- getLR()$site

  if(unlist==F){
    #fingerprinting-30, cryptominer-16,
    #session-replay in analytics(later)
    x <- disconnect$categories[[1]]
    adsDomains <-unlist(x) 
    ind <- grep("fingerprinting",gsub(".*\\.","",names(adsDomains)))
    fingerprinting <- names(adsDomains)[ind]
    fingerprinting <- tolower(gsub(".fingerprinting","",fingerprinting))
    adsDomains <- adsDomains[-ind]  
    
    ind <- grep("cryptominer",gsub(".*\\.","",names(adsDomains)))
    cryptominer <- names(adsDomains)[ind]
    cryptominer <- tolower(gsub(".cryptominer","",cryptominer))
    
    advertising <- as.character(adsDomains[-ind])
    
    
    disconnectAds <- list("First-Party"=fp,"Advertising"=advertising,
                          "Fingerprinting"=fingerprinting
                          ,"Cryptominer"=cryptominer)
    disconnectDomains <- c()
    disconnectDomains <- lapply(disconnect$categories[2:5], function(x){
      domains <- unique(as.character(unlist(x)))
      domains
    })
    
    disconnectDomains <- c(disconnectAds,disconnectDomains)
    disconnectDomains
  }else{
    #panos list, 3 cols  
    x <- disconnect[[1]]
    disconnectDomains <- sapply(disconnect, function(x){
      unlist(x)
    })
    disconnectDomains <- t(disconnectDomains)
    disconnectDomains <- as.data.frame(cbind(disconnectDomains,url=rownames(disconnectDomains)))
    colnames(disconnectDomains) <- c("type","topDomain","topURL","url")
    
    #diconnectme list
    disconnectDomains2 <- melt(disconnect2$categories)
    disconnectDomains2 <- disconnectDomains2[,c(6,4,3,1)]
    colnames(disconnectDomains2) <- c("type","topDomain","topURL","url")
    disconnectDomains2 <- rbind(disconnectDomains2,data.frame("type"="First-party","topDomain"=fp,"topURL"=fp,"url"=fp,stringsAsFactors = F))
    addDomain <- setdiff(disconnectDomains2$url,disconnectDomains[,4])
    #j <- join(disconnectDomains,disconnectDomains2,"url")
    disconnectDomains <- rbind(disconnectDomains,
                               disconnectDomains2[disconnectDomains2$url%in%addDomain,])
    #store merged 147 Panos and disconnect. Other way is 15
    #also store fingerPrint/crpto/performance seperate.
    fpctP <- disconnectDomains[disconnectDomains$url=="true",]
    #write.csv(fpctP,"",row.names = F)
    disconnectDomains <- disconnectDomains[!disconnectDomains$url=="true",]
    disconnectDomains$type[disconnectDomains$type=="Disconnect"] <- "Social"
    disconnectDomains <- disconnectDomains[grep("[.]",disconnectDomains$url),]
    #write.csv(disconnectDomains,"servicesDisconnectP.csv",row.names = F)
  }
  disconnectDomains
}


######Cookies table metadata#####
#checking inter party cookies, session cookies,changed
#profiles vs js jsCookies

cookiesMeta <- function(jsCookie,pCookie,ind){
  
  visitId <- jsCookie$visit_id
  cookJs<- jsCookie[visitId%in%which(lh$political_category=="right"),]
  rtMeta <- metaCook(cookJs)
  metaLh <- data.frame(id=names(rtMeta),value=as.numeric(rtMeta),
                       var="Right",ind=ind,stringsAsFactors = F)
  cookJs<- jsCookie[visitId%in%which(lh$political_category=="left"),]
  
  lfMeta <- metaCook(cookJs)
  metaLh <- rbind(metaLh,data.frame(id=names(lfMeta),
                                    value=as.numeric(lfMeta),
                                    var="Left",ind=ind,stringsAsFactors = F))
  metaLh  
  
}


metaCook <- function(cookJs){
  len <- nrow(cookJs)
  change <- sortedTable(cookJs$change)[2:3]
  httpOnly <- length(which(cookJs$is_http_only==1))
  #sessions
  session <- length(which(cookJs$is_session==1))
  #.XXX cookies, else 159.203.165.175
  domain <- length(which(cookJs$is_domain==1))
  #paypay etc
  secure <- length(which(cookJs$is_secure==1))
  c(change,"HttpOnly"=httpOnly,"Session"=session,
    "Domain"=domain,"Secure"=secure)/len*100  
}


#######check cookies domains Percentage #####
#using all 1332 domains: OLD
#using all list file
#now configured for types as well
cookiesDiff <- function(domain="send urls/domains in col 3",
                        allDomains,ind,http=F,filter=NA){
  
  #using First party as well. to see which fp track which persona
  
  if(http==T){
    domain[,3] <- fileExplodeName(domain[,3],using = "/",param=3)
  }
  if(!is.na(filter)){
    #here typeD to map rawhost to topURL
    typeD <- allDomains[match(domain[,3],allDomains$host),]
    typeD <- cbind(domain,typeD)
    typeD <- typeD[which(typeD$type==filter),]
  }
  
  #Domain which is placing ads
  uqTopURL <- unique(allDomains$topURL)
  cat(length(unique(typeD$topDomain)),"/",length(uqTopURL),",")
  
  #selecting from visit ID LR. Making list in the end
  left <- typeD[typeD$visit_id%in%lhVisitID(),]
  right <- typeD[typeD$visit_id%in%lhVisitID(party = "right"),]
  #normalising count from total
  leftNorm <- length(which(domain$visit_id%in%lhVisitID()))
  rightNorm <- length(which(domain$visit_id%in%lhVisitID(party = "right")))
  
  #using all left, all top. -1 from counted and normalise it
  count <- count(c(left$topURL,uqTopURL))
  #left <- (count[,2]-1)/length(left$topURL)*100
  left <- (count[,2]-1)/leftNorm*100
  names(left) <- count[,1]
  left <- sort(left,decreasing = T)
  
  count <- count(c(right$topURL,uqTopURL))
  #right <- (count[,2]-1)/length(right$topURL)*100
  right <- (count[,2]-1)/rightNorm*100
  names(right) <- count[,1]
  #using colnames as left
  #roughly 11.4 JSC left and 29.0 right,factor- 67 and 74
  #roughly 24.3k Http left and  62.4K http right - 143 and 161
  right <- right[match(names(left),names(right))]#sort(right,decreasing = T)

  allCookies <- list(left,right)
  names(allCookies) <- paste0(c("Left_","Right_"),ind)
  allCookies
}

######get LH visit IDs####
#carefull, visitID 1 for spectator.org
lhVisitID <- function(party="left",namesId=NA){
  lr <- getfiles()
  id <- 557-which(lr$political_category==party)
  if(length(namesId)>1){
    id <- lr$site[557-as.numeric(namesId)]
  }
  id
}

#####Check cookies Count##### Testing all df.
calLRCount <- function(cookie="visitIDs",allDomains,ind,filter=NA){
  #not conf. for all like http
  if(!is.na(filter)){
    typeD <- allDomains[match(cookie[,3],allDomains$host),]
    typeD <- cbind(cookie,typeD)
    cookie <- cookie[which(typeD$type==filter),]
  }
  cookie <- cookie$visit_id
  allCookiesVisit <-c(cookie,1:556)
  allCookiesVisit <- sortedTable(allCookiesVisit,sorting = F)-1
  left <- list(allCookiesVisit[names(allCookiesVisit)%in%lhVisitID()])
  right <- list(allCookiesVisit[names(allCookiesVisit)%in%lhVisitID(party = "right")])
  
  allCookies <- c(left,right)
  names(allCookies) <- paste0(c("Left_","Right_"),ind)
  allCookies
}


#####All in One tansformation####
allInOneT <- function(listCount){
  nameList <- names(listCount)
  nameList <- fileExplodeName(nameList,param = 4)
  uqName <- unique(nameList)
  newList <- c()
  cat("All in One ON") 
  #cat(", removing zeros\n")
  i <- uqName[1]
  for (i in uqName){
    ind <- which(i==nameList)
    unlistNew <- sort(as.numeric(unlist(listCount[ind])),decreasing = T)
    # zeroInd <- which(unlistNew==0)
    # if(length(zeroInd)>0){
    #   unlistNew <- unlistNew[-zeroInd]
    # }
    newList <- c(newList,list(unlistNew))
  }
  #cat(sapply(newList,length))
  names(newList) <- uqName
  newList
}

#####calulate median#####
#Multiplt list to average list
#carefull with name averaging, also update list visit ids
calculateMedian <- function(cookiesCount,colnames=NA,max=F){
  namesList <- fileExplodeName(names = names(cookiesCount),using = "_",param = 4)
  #count(namesList)
  uqNamesList <- unique(namesList)
  newCookiesCount <- c()
  newName <- c()
  i <- uqNamesList[1]
  for (i in uqNamesList){
    #cat("\n ",i)
    ind <- grep(paste0("_",i),names(cookiesCount),value = T)
   # cat("\nAveraging-",length(ind))
    df <- do.call(cbind,cookiesCount[ind])
    if(max==T){
      cat("using max\n")
      crawlMid <- rowMax(df = df)
    }else{
        crawlMid <- rowMedians(df, na.rm=TRUE)
    }
    newCookiesCount <- rbind(newCookiesCount,crawlMid)
    newName <- c(newName,i)
  }
  newName  
  cat("Dim",dim(newCookiesCount),"sum",sum(newCookiesCount))
  rownames(newCookiesCount) <-newName
  #use NA for colnames to be news websites and not domains
  if(is.na(colnames)){
    colnames(newCookiesCount) <-lhVisitID(namesId=colnames(newCookiesCount))
  }else{
    colnames(newCookiesCount) <-names(cookiesCount[[1]])
  }
  newCookiesCount
}


####export Medians of LR Visit Ids#####  
exportLR <- function(listLR="listOfLRIds",name="test.csv",path=".",
                     median=T,colnames=NA,max=F){
  lf <- grep("Left",names(listLR))
  rt <- grep("Right",names(listLR))
  if(median==T){
    #cookiesCount <- cookiesCount[lf]
    lfCount <- calculateMedian(cookiesCount=listLR[lf],colnames = colnames)
    rtCount <- calculateMedian(listLR[rt],colnames = colnames)
  }else if(max==T){
    lfCount <- calculateMedian(cookiesCount=listLR[lf],colnames = colnames,max=T)
    rtCount <- calculateMedian(listLR[rt],colnames = colnames,max=T)
  }else{
    lfCount <- do.call(rbind,listLR[lf])
    rownames(lfCount) <- gsub("Left_","",rownames(lfCount)) 
    rtCount <- do.call(rbind,listLR[rt])
    rownames(rtCount) <- gsub("Right_","",rownames(rtCount))
    if(is.na(colnames)){
      colnames(lfCount) <-lhVisitID(namesId=colnames(lfCount))
      colnames(rtCount) <-lhVisitID(namesId=colnames(rtCount))
    }else{
      colnames(lfCount) <-colnames(lfCount)
      colnames(rtCount) <-colnames(rtCount)
    }
  }
  write.csv(lfCount,paste0(path,"/left",name))
  write.csv(rtCount,paste0(path,"/right",name))
}



#########Plotting Functions#####
getLRVisitIDPlot <- function(file1,file2,xLab="x",yLab="CDF",
                             figure="cdf",log=F,allType=NA,
                             party="all",color=NA,allInOne=F,
                             smooth=F,http=F,allJSDomains=NA){
  
  {
    # file1 = "./csv_results/leftJsCookiesCountMedian.csv"
  # file2 = "./csv_results/rightJsCookiesCountMedian.csv"
  # file1 = "./csv_results/leftJsCookiesDomainMedian.csv"
  # file2 = "./csv_results/rightJsCookiesDomainMedian.csv"
 # file1 = "./csv_results/leftJsCookiesCountAll.csv"
# file2 = "./csv_results/rightJsCookiesCountAll.csv"
    # file1 = "./csv_results/leftJsCookiesCountAdsAll.csv"
    # file2 = "./csv_results/rightJsCookiesCountAdsAll.csv"
    # file1 = "./csv_results/leftJsCookiesDomainAll.csv"
    # file2 = "./csv_results/rightJsCookiesDomainAll.csv"

    }
  {
  cat("removing location specific and first-party for JS")
  count <- fileRead(file1)
  count <- count[,!colnames(count)%in%allJSDomains$url[allJSDomains$type=="first-party"]]
  
  names <- paste0("L:",count[,1])
  rownames(count) <- count[,1]
  rownames(count) <- gsub(".sqlite","",rownames(count))
  count <- count[,-1]
  count <- mergeLocation(matrix=count,list = F)
  listCount <- as.list(data.frame(t(count),stringsAsFactors = F))
  names(listCount) <- rownames(count)
  
  count2 <- fileRead(file2)
  count2 <- count2[,!colnames(count2)%in%allJSDomains$url[allJSDomains$type=="first-party"]]
  
  names2 <- paste0("R:",count2[,1])
  rownames(count2) <- count2[,1]
  rownames(count2) <- gsub(".sqlite","",rownames(count2))
  count2 <- count2[,-1]
  count2 <- mergeLocation(matrix=count2,party="R:",list = F)
  listCount2 <- as.list(data.frame(t(count2),stringsAsFactors = F))
  names(listCount2) <- rownames(count2)
  }
  #96 to 20 DF return, also update name
  if(allInOne==T){
    listCount <- allInOneT(listCount)
    names(listCount) <- paste0("L:",names(listCount))
    listCount2 <- allInOneT(listCount2)
    names(listCount2) <- paste0("R:",names(listCount2))
  }
  names(listCount) <- gsub(".sqlite","",names(listCount))
  names(listCount2) <- gsub(".sqlite","",names(listCount2))
  
  if(figure=="cdf"){
    plotCDFLR(cdf = c(listCount,listCount2),log = log,xLab =xLab,yLab = yLab,
              smooth=smooth,legPos=c(0.7,0.3),label="Party")
  }else if(figure=="ks"){
    cat("Using-\n",file1,"\n",file2)
    checkPvalue(crawlRun = c(listCount,listCount2),
                test="ks",scaleName = xLab)
  }else if(figure=="gini"){
    cat("Using-\n",file1,"\n",file2)
    giniL <- unlist(lapply(listCount1, gini))
    giniR <- unlist(lapply(listCount2, gini))
    df <- data.frame(id="Left",value=as.numeric(giniL),
                     var="Left")
    df <- rbind(df,data.frame(id="Right",value=as.numeric(giniR),
                     var="Right"))
    df <- df[order(df$value,decreasing = T),]
    plotBoxGGP(df,xLab =xLab,yLab = "Gini Index",
               color = color,yMargin=1)
  }else if(figure=="tt"){
    checkPvalue(crawlRun = c(listCount,listCount2),
                test="t",scaleName = xLab)
  }else if(figure=="box"){
    cat("Using-\n",file1,"\n",file2)
    valueL <- melt(t(count))
    valueR <- melt(t(count2))
    df <- data.frame(var="Left",value=valueL$value,
                     id=valueL$Var2)
    df <- rbind(df,data.frame(var="Right",value=valueR$value,
                              id=valueR$Var2))
   # cat("removing zeros...\n")
    #df <- df[-which(df$value==0),]
    plotBoxGGP(df,xLab =xLab,yLab = "Percentage",
               color = color,yMargin=1,legPos = "right")  
   }else if(figure=="jaccard"){
    listCount[[1]]
    #calculate jaccard for all possible combinations of 2
    
    allJaccard <- c()
    i <- j <- 1
    for(i in 1:length(allList)){
      jaccard <- c()
      for(j in 1:length(allList)){
        one <- as.numeric(allList[[i]])
        two <- as.numeric(allList[[j]])
        jaccard <- c(jaccard,length(which(one&two))/length(which(one|two)))
      }
      allJaccard <- rbind(allJaccard,jaccard)
    }      
    colnames(allJaccard) <- rownames(allJaccard) <- c(names(listCount),names(listCount2))
    heatmap(allJaccard)
    write.csv(1-allJaccard,"csv_results/OneMinusJaccard.csv")
    #self heat map
    #print(plotHeatGGP(melt(rbind(allJaccard[21:40,1:20],allJaccard[1:20,21:40])),legPos = "right"))
    # heat map
    print(plotHeatGGP(melt(1-allJaccard),legPos = "right"))
  }else if(figure=="type"){
    #Nw use from normalise factor, only conf. for http and jsc
    if(http==T){
    #taking average of 35.5k and 60 cookies 
    #roughly 9.918 JSC left and 25.23 right,factor- 58 and 65
    #roughly 21.1k Http left and  54.2k http right - 125 and 140
      listCount <- lapply(listCount, function(x){x*125/100})
      listCount2 <- lapply(listCount2, function(x){x*140/100})
    }else{
      listCount <- lapply(listCount, function(x){x*58/100})
      listCount2 <- lapply(listCount2, function(x){x*65/100})
    }
    dfLR <- do.call(rbind,c(listCount,listCount2))
    cat("Using lists-",dim(dfLR))
    colnames(dfLR) <- names(count)
    dfLR <- fixHttpHeader(dfLR,allType = allType)
    allTypeFile <- fileRead(allType)
    #carefull, colnames space[-] is removed with a [.]
    #also update "X"
    #also top URL match- Miss-leading.
    ind <- match(gsub("[^a-zA-Z0-9]","",colnames(dfLR)),
                 gsub("[^a-zA-Z0-9]","",allTypeFile$topURL))
    cat(colnames(dfLR)[(which(is.na(ind)))],sum(dfLR[,(which(is.na(ind)))])/40)
    typeName <- allTypeFile$type[ind]
    typeCount <- sortedTable(typeName)
    barCount <- c()
    fileName <- fileExplodeName(rownames(dfLR),using = ":")
    i <- 1
    for(i in 1:6){
      cat(i)
      sum <- rowSums(dfLR[,which(typeName%in%names(typeCount[i]))]) 
      barCount <- rbind(barCount,data.frame(id=names(typeCount[i]),value=as.numeric(sum),var=c(rep("Left",20),rep("Right",20)),stringsAsFactors = F))
    }
    print(barCount)
    #preset color
    plotBarGGP2(bar = barCount,xLab = xLab,color = 1,yLab = "Count per website",label = "Party",sort = F)
  }else if(figure=="zscore"|figure=="rankingBox"){
    #take base and check z-score and plot standard error
    #remove zero base col, OLD,
    #remove all with zero colsums for left
    #shapiro.test(listCount[[1]]) p<0.00001, W=0.4
    ind <- grep("Base",names(listCount))
    #count <- count[,!colSums(count)==0]
    baseL <- as.numeric(count[ind,])
    indZero <- which(baseL==0)
    baseL <- baseL[-indZero]
    #baseL <- baseL[c(1:50)]
    #names <- names[-ind] 
    mean <- mean(baseL)
    median <- median(baseL)
    sd <- sd(baseL)
    allL <- count[-ind,-indZero]
    #allL <- count[-ind,]
    cat(mean,":",sd,":",median)
    
    ind <- grep("Base",names(listCount2))
    #count2 <- count2[,!colSums(count2)==0]
    baseR <- as.numeric(count2[ind,])
    indZero <- which(baseR==0)
    baseR <- baseR[-indZero]
    #baseR <- baseR[c(1:50)]
    #names2 <- names2[-ind] 
    allR <- count2[-ind,-indZero]
    #allR <- count2[-ind,]
    mean2 <- mean(baseR)
    sd2 <- sd(baseR)
    cat(mean2,":",sd2,":",median(baseR))
    
    df <- c()    
    #base L =494 and base R 760 non zeros
    i <- 1
    for(i in (1:8)){
      nameLR <- gsub("L:|.sqlite","",names(listCount)[i])
      cat(nameLR,"\n")
      if(party=="all"|party=="left"){
        cat(names[i],"\n")
        countCol <- allL[i,]
        #countCol <- as.numeric(countCol[-which(countCol==0)])
        cat(length(countCol))
        #zScL <- (countCol-mean)/sd
        zScL <- (countCol-baseL)/baseL*100
        df <- rbind(df,data.frame(value=as.numeric(zScL),
                                   id=nameLR,var="Left"))
      }
      if(party=="all"|party=="right"){
        cat(names2[i],"\n")
        countCol <- allR[i,]
        #countCol <- as.numeric(countCol[-which(countCol==0)])
        cat(length(countCol))
        #zScR <- (countCol-mean2)/sd2
        zScR <- (countCol-baseR)/baseR*100
        df <- rbind(df,data.frame(value=as.numeric(zScR),
                                   id=nameLR,var="Right"))
      }
    }
    cat("no order to follow")  
    # order <- order(sapply(df, function(x){median(x$value)}))
    # df <- df[order]
    if(figure=="rankingBox"){
      
    }else
    plotBoxGGP(df,xLab =xLab,yLab = "Percentage Difference",
               color=color,label = "Party",
              yMargin = 1.5,legPos = "right",melt = F)
    # box <- df
    # box$var <- factor(box$var)
    # oind <- order(as.numeric(by(box$value, box$var, median)))    
    # box$var <- ordered(box$var, levels=levels(box$var)[oind])
    # p<-ggplot(box, aes(x=id, y=value,color=var)) +
    #   geom_boxplot(outlier.shape = NA)+
    #   theme(axis.text.x = element_text(angle = 60, hjust = 1),
    #         legend.position="right")
    # if(!is.na(color)[1])
    #   p <- p+ scale_color_manual(breaks=c("Left","Right"),values=c("blue","red"))
    # p
  }
}

##########Http topURL to top domain#####
fixHttpHeader <- function(matrix,allType,merge=F){
  "Later"
  #matching colnames of matrix with type 
  allTypeFile <- fileRead(allType)
  ind <- grep("http...",colnames(matrix))
  indFix <- grep("http...",colnames(matrix),value = T)
  indFix <- gsub("...","://",indFix,fixed = T)
  x <- indFix[1]
  indFix <- lapply(indFix, function(x){
    last <- stri_sub(x,-1,-1)
    if(last=="."){
      x <- paste0(stri_sub(x,from = 0,to = -2),"/")
    }
    x
  })
  if(length(grep("http://narrative.io.2/",indFix))>0)
    indFix[[which(indFix=="http://narrative.io.2/")]] <- "http://narrative.io/2/"
  m <- match(indFix,allTypeFile$topDomain)
  cat("Found ",length(indFix),"/",length(m))
  #finding replacement
  colnames(matrix)[ind] <- allTypeFile$topURL[m]
  multiInd <-sortedTable(colnames(matrix))[sortedTable(colnames(matrix))>1]
  cat("multi", multiInd)
  mutiMatch <- match(unique(colnames(matrix)),colnames(matrix))
  matrix[,mutiMatch]
}

#####NMF Clsutering#####

getNMFEstim <- function(file1="Send file 1 median",file2,allType,filterLoc=T){
    file1 <- fileRead(file1)
    file2 <- fileRead(file2)
    rownames(file1) <- paste0("L:",file1[,1])
    file1 <- file1[,-1]
    rownames(file2) <- paste0("R:",file2[,1])
    file2 <- file2[,-1]
    rownames(file1) <- gsub(".sqlite","",rownames(file1))
    rownames(file2) <- gsub(".sqlite","",rownames(file2))
    if(filterLoc==T){
      file1 <- mergeLocation(matrix = file1,party = "L:",list = F)
      file2 <- mergeLocation(matrix = file2,party = "R:",list = F)
    }
    
  matrix <- rbind(file1,file2)
  #names <- c("Ads","Ana","Con","FP","Other","Soc")
  allTypeFile <- allJSDomains
  ind <- match(gsub("[^a-zA-Z0-9]","",colnames(matrix)),
               gsub("[^a-zA-Z0-9]","",allTypeFile$topURL))
  cat(colnames(matrix)[(which(is.na(ind)))],sum(matrix[,(which(is.na(ind)))])/40)
  typeName <- allTypeFile$type[ind]
  typeCount <- count(typeName)
  estimAll <- c()
  i <- typeCount$x[1]
  for(i in typeCount$x[1:6]){
  #  readFiles <- grep(i,filesLR,value = T) 
  #cat("\nreading files-\n",readFiles)
  #cat("\nrunning NMF-\n",i)
  #selection testing, taking full file
  #indCols <- allJSDomains$topURL[allJSDomains$type=="first-party"]
  indCols <-   allJSDomains$topURL[allJSDomains$type==i]
  nmfMat <- matrix[,which(colnames(matrix)%in%indCols)]
  #no difference with this matrix as well  
  #columns <5% contributions
  nmfMat <- nmfMat[,which(!colSums(nmfMat)==0)] 
  rowSumNMF <- which(rowSums(nmfMat)==0)
  if(length(rowSumNMF)>0){
      cat("Found zero rows",length(rowSumNMF))
      nmfMat <- nmfMat[-rowSumNMF,] 
  }
    cat("\nDim-",dim(nmfMat),"\n")
    estim <- nmf(nmfMat, 2:3, nrun=20, seed=123456)
    cat(i,"NMF",estim$measures$rss)
    estimAll <- c(estimAll,estim)
  }
  estimAll
}

getAllTypeRead <- function(name){
  allTypeFile <- fileRead(name)
  typeName <- allTypeFile$type
  typeCount <- count(typeName)
  typeCount
}


####get LH rank box summary####
getLHBoxSummary <- function(df,id){
  ind <- df[id==df$id,]
  r <- ind$value[which(ind$var=="Right")]
  r <- sort(r,decreasing = F)
  cat("\nRight=",r[ceiling(length(r)*c(0.05,0.25,0.50,0.75,0.95))])
  l <- ind$value[which(ind$var=="Left")]
  l <- sort(l,decreasing = F)
  cat("\nLeft=",l[ceiling(length(l)*c(0.05,0.25,0.50,0.75,0.95))])
}