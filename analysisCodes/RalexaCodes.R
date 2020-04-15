#########New ranking crawl for HPWs from Alexa2019####################################

#read websites from FileHPWsAlexaRankCookies
partisan <- read.csv("FileHPWsAlexaRankCookies.csv")
rankNew <-c()
i <- 555

for(i in 1:length(partisan$site)){
  cat(i,partisan$site[i])
  crawl <- NA
  count <- 0
  try({
    crawl <- read_html(paste0("https://www.alexa.com/siteinfo/",partisan$site[i]))
    crawl <- html_text(html_nodes(crawl,".globleRank .metrics-data"))
    crawl <- as.numeric(gsub("[^0-9]","",crawl))
    cat(crawl,"\n")
    indC <- which(partisan$site[i]==colnames(jscCount))
    if(length(indC)>0)
      count <- jscCount[19,indC]
    })
  
    if(!is.na(crawl)&length(indC)>0){
      rankNew <- rbind(rankNew,data.frame(site=partisan$site[i],
                            rankGlobal=crawl,jsCookies=jscCount[19,indC],stringsAsFactors = F)) 
    }else{
      rankNew <- rbind(rankNew,data.frame(site=partisan$site[i],rankGlobal=NA,
                                          jsCookies=count,stringsAsFactors = F)) 
      cat("less info\n")
    }
}

rankNew <- cbind(rankNew,"party"=partisan$political_category)

#write.csv(rankNew,"alexaRankAndCookies2019.csv")
