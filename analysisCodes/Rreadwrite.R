#Power house of exports, including all counts lists

source("RFunctions.R")

library("RSQLite")
library(jsonlite)
library(robustbase)
library(anytime)
library(NMF)
library(rvest)
library(plyr)
library(stringi)


#####sample import###########
## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), 
                 dbname="XXX.sqlite")
## list all tables
tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

lDataFrames <- vector("list", length=length(tables))
i <- 1
## create a data.frame for each table
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
  cat(tables[[i]],nrow(lDataFrames[[i]]),"\n")  
}

dfcrawl <- lDataFrames[[11]]
dfHTRed <- lDataFrames[[4]]
dfHTReq <- lDataFrames[[5]]
dfHTRes <- lDataFrames[[6]]
dfJS <- lDataFrames[[7]]
dfJSC <- lDataFrames[[8]]
dfPC <- lDataFrames[[9]]

###getting counts of LR from tables####
length(dfHTRes$visit_id)-length(dfHTRes$visit_id[dfHTRes$visit_id%in%lhVisitID()])
length(dfJSC$visit_id)-length(dfJSC$visit_id[dfJSC$visit_id%in%lhVisitID()])

expFile <- c(2,4:9)
i <- expFile[1]
for(i in expFile){
  tableC <- lDataFrames[[i]]
  write.csv(tableC,paste0(tables[[i]],".csv"))
  cat("exporting",tables[[i]],nrow(tableC))
}

########read all cookies/https, check unique####

path <- "XXX"
files <- list.files(path)

http <- T

allUq <- c()
x <- files[1]
l <- lapply(files, function(x){
  f <- read.csv(file = paste0(path,x),stringsAsFactors = F)
  cat(x,"\n")
  if(http==T)
    f[,3] <- fileExplodeName(f[,3],using = "/",param=3)
  allUq <<- unique(c(allUq,f[,3]))
})



#all         
# 2.55 M redirects
# 8.27 M request
# 8.07 M responses
# 16.55 M js
# 3.48 M jsc
# 1.7 M pc

#base se % Diff
