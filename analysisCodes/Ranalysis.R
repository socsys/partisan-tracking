source("RFunctions.R")

##########plots#######

getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesCountMedian.csv",
         file2 = "./csv_results/rightJsCookiesCountMedian.csv",
                 xLab = "Cookies count (Log10)",
                 yLab = "CDF",log = T,smooth=T)
plotExportGGP(name = "jscCountShaded",type = ".png",figsize = 1.5,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftPCookiesCountMedian.csv",
                 file2 = "./csv_results/rightPCookiesCountMedian.csv",
                 xLab = "Profile cookies count (Log10)",
                 yLab = "CDF",log = T)
plotExportGGP(name = "pCount",figsize = 3,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftJsCountMedian.csv",
                 file2 = "./csv_results/rightJsCountMedian.csv",
                 xLab = "Javascript count (Log10)",
                 yLab = "CDF",log = T)
plotExportGGP(name = "jCount",figsize = 3,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftHttpResCountMedian.csv",
                 file2 = "./csv_results/rightHttpResCountMedian.csv",
                 xLab = expression('Http Responses count Log'[10]),
                 yLab = "CDF",log = T)
plotExportGGP(name = "htResCount",figsize = 3,ggp = T,type = ".png")

##KS Test plots, domain counts
getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesCountMedian.csv",
                 file2 = "./csv_results/rightJsCookiesCountMedian.csv",
                 xLab = "p-Value",
                 yLab = "CDF",figure = "ks")
plotExportGGP(name = "jscCountKS",figsize = 3,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftPCookiesCountMedian.csv",
                 file2 = "./csv_results/rightPCookiesCountMedian.csv",
                 xLab = "p-Value",
                 yLab = "CDF",figure = "ks")
plotExportGGP(name = "pCountKS",figsize = 3,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftJsCountMedian.csv",
                 file2 = "./csv_results/rightJsCountMedian.csv",
                 xLab = "p-Value",
                 yLab = "CDF",figure = "ks")
plotExportGGP(name = "jCountKS",figsize = 3,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftHttpResCountMedian.csv",
                 file2 = "./csv_results/rightHttpResCountMedian.csv",
                 xLab = "p-Value",
                 yLab = "CDF",figure = "ks")
plotExportGGP(name = "htResCountKS",figsize = 3,ggp = T)

####Domain % plot CDF
getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesDomainMedian.csv",
                 file2 = "./csv_results/rightJsCookiesDomainMedian.csv",
                 xLab = "Javascript cookies Domains Percent",
                 yLab = "CDF",log = T)
plotExportGGP(name = "jscDomain",figsize = 3,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftPCookiesDomainMedian.csv",
                 file2 = "./csv_results/rightPCookiesDomainMedian.csv",
                 xLab = "Profile cookies Domains",
                 yLab = "CDF")
plotExportGGP(name = "pCount",figsize = 3,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftJsDomainMedian.csv",
                 file2 = "./csv_results/rightJsDomainMedian.csv",
                 xLab = "Javascript count (Log10)",
                 yLab = "CDF")
plotExportGGP(name = "jCount",figsize = 3,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftHttpResDomainMedian.csv",
                 file2 = "./csv_results/rightHttpResDomainMedian.csv",
                 xLab = "Http Responses count (Log10)",
                 yLab = "CDF",log = T)
plotExportGGP(name = "htResDomainM50",figsize = 3,ggp = T)

##KS Test plots, domain Percent
getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesDomainMedian.csv",
                 file2 = "./csv_results/rightJsCookiesDomainMedian.csv",
                 xLab = "KS test\nstatistic",
                 yLab = "CDF",figure = "ks",allJSDomains=allJSDomains)
plotExportGGP(name = "jscDomainKSGreat",figsize = 3,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesCountAll.csv",
                 file2 = "./csv_results/rightJsCookiesCountAll.csv",
                 xLab = "KS test\nstatistic",
                 yLab = "CDF",figure = "ks",allInOne = T)
plotExportGGP(name = "jscCountKSGreatAll",figsize = 3,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftPCookiesDomainMedian.csv",
                 file2 = "./csv_results/rightPCookiesDomainMedian.csv",
                 xLab = "p-Value",
                 yLab = "CDF",figure = "ks")
plotExportGGP(name = "pDomainKS",figsize = 3,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftJsDomainMedian.csv",
                 file2 = "./csv_results/rightJsCountMedian.csv",
                 xLab = "p-Value",
                 yLab = "CDF",figure = "ks")
plotExportGGP(name = "jDomainKS",figsize = 3,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftHttpResDomainMedian.csv",
                 file2 = "./csv_results/rightHttpResDomainMedian.csv",
                 xLab = "p-Value",
                 yLab = "CDF",figure = "ks")
plotExportGGP(name = "htResDomainKS",figsize = 3,ggp = T)


##Type Barplots
getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesDomainMedian.csv",
                 file2 = "./csv_results/rightJsCookiesDomainMedian.csv",
                 allType="../Telefonica/allJSDomainsTypeUpdated.csv",
                 xLab = NULL,
                 yLab = "CDF",figure = "type")
plotExportGGP(name = "jscTypeWebsite",figsize = 1.5,ggp = T,type = ".png")

# getLRVisitIDPlot(file1 = "./csv_results/leftPCookiesDomainMedian.csv",
#                  file2 = "./csv_results/rightPCookiesDomainMedian.csv",
#                  allType="../Telefonica/allPCDomainsType.csv",
#                  xLab = NULL,
#                  yLab = "CDF",figure = "type")
# plotExportGGP(name = "pcType",figsize = 2,ggp = T)

# getLRVisitIDPlot(file1 = "./csv_results/leftJsDomainMedian.csv",
#                  file2 = "./csv_results/rightJsDomainMedian.csv",
#                  allType="../Telefonica/allJSDocDomainsType.csv",
#                  xLab = NULL,
#                  yLab = "CDF",figure = "type")
# plotExportGGP(name = "jsdType",figsize = 2,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftHttpResDomainMedian.csv",
                 file2 = "./csv_results/rightHttpResDomainMedian.csv",
                 allType="../Telefonica/allHTTPResDomainsType.csv",
                 xLab = NULL,
                 yLab = "CDF",figure = "type",http = T)
plotExportGGP(name = "htrespTypeWebsite",figsize = 1.5,ggp = T,type = ".png")

###Z-score: OLD, not % diff######

getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesCountMedian.csv",
                 file2 = "./csv_results/rightJsCookiesCountMedian.csv",
                 xLab = "Persona",
                 yLab = "CDF",figure = "zscore",party = "all",
                 color = c("red","blue"))

plotExportGGP(name = "jscDiffCountAll",figsize = 2,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesCountMedian.csv",
                 file2 = "./csv_results/rightJsCookiesCountMedian.csv",
                 xLab = "Cookies Count",
                 yLab = "CDF",figure = "zscore",party = "left",color = "blue")
plotExportGGP(name = "jscCountZscoreLeft",figsize = 2,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesCountMedian.csv",
                 file2 = "./csv_results/rightJsCookiesCountMedian.csv",
                 xLab = "Cookies Count",
                 yLab = "CDF",figure = "zscore",party = "right")
plotExportGGP(name = "jscCountZscoreRight",figsize = 2,ggp = T)

#domains box plot
getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesDomainMedian.csv",
                 file2 = "./csv_results/rightJsCookiesDomainMedian.csv",
                 xLab = "Personas",
                 yLab = "CDF",figure = "zscore",party = "all",color = c("red","blue"),allJSDomains = allJSDomains)
plotExportGGP(name = "./figures/domains/jscDiffDomainAll",figsize = 2,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftPCookiesDomainMedian.csv",
                 file2 = "./csv_results/rightPCookiesDomainMedian.csv",
                 xLab = "Personas",
                 yLab = "CDF",figure = "zscore",party = "right",color = c("red","blue"))
plotExportGGP(name = "pDiffDomainAll",figsize = 2,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesDomainMedian.csv",
                 file2 = "./csv_results/rightJsCookiesDomainMedian.csv",
                 xLab = "Left Persona",
                 yLab = "CDF",figure = "zscore",party = "left",color = "red")

plotExportGGP(name = "jscDiffDomainLeft",figsize = 2,ggp = T)
getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesDomainMedian.csv",
                 file2 = "./csv_results/rightJsCookiesDomainMedian.csv",
                 xLab = "Right Persona",
                 yLab = "CDF",figure = "zscore",party = "right")

plotExportGGP(name = "jscDiffDomainRight",figsize = 2,ggp = T)

#######KS and t for all crawls data####
getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesCountAll.csv",
                 file2 = "./csv_results/rightJsCookiesCountAll.csv",
                 xLab = "Personas",yLab = "CDF",
                 figure = "cdf",allInOne=T,log = T)

getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesDomainAll.csv",
                 file2 = "./csv_results/rightJsCookiesDomainAll.csv",
                 xLab = "Personas",yLab = "CDF",
                 figure = "cdf",allInOne=T,log = T)

getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesCountAll.csv",
                 file2 = "./csv_results/rightJsCookiesCountAll.csv",
                 xLab = "KS test\nstatistic",yLab = "CDF",
                 figure = "ks",allInOne=T,log = T)
plotExportGGP(name = "jscCountKSGreatAllVal",figsize = 3,ggp = T)

getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesDomainAll.csv",
                 file2 = "./csv_results/rightJsCookiesDomainAll.csv",
                 xLab = "KS test\nstatistic",yLab = "CDF",
                 figure = "ks",allInOne=T,log = T,allJSDomains=allJSDomains)
plotExportGGP(name = "./figures/domains/jscDomainKSGreatAllVal",
              figsize = 2,ggp = T)


getLRVisitIDPlot(file1 = "./csv_results/leftPCookiesCountAll.csv",
                 file2 = "./csv_results/rightPCookiesCountAll.csv",
                 xLab = "KS test\nstatistic",yLab = "CDF",
                 figure = "ks",allInOne=T,log = T)
plotExportGGP(name = "pCountKSGreatAllVal",figsize = 3,ggp = T)


getLRVisitIDPlot(file1 = "./csv_results/leftPCookiesDomainAll.csv",
                 file2 = "./csv_results/rightPCookiesDomainAll.csv",
                 xLab = "KS test\nstatistic",yLab = "CDF",
                 figure = "ks",allInOne=T,log = T)

plotExportGGP(name = "pDomainKSGreatAllVal",figsize = 3,ggp = T)

####More plots for type of tables###
getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesCountAdsAll.csv",
                 file2 = "./csv_results/rightJsCookiesCountAdsAll.csv",
                 xLab = "KS test\nstatistic",yLab = "CDF",
                 figure = "ks",allInOne=T,log = T)
plotExportGGP(name = "jscAdsKSGreatAllVal",figsize = 3,ggp = T)

##########Websites ranking box#######
#########Jaccard##########
getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesDomainMedian.csv",
                 file2 = "./csv_results/rightJsCookiesDomainMedian.csv",
                 xLab = "Cookies Count",
                 yLab = "CDF",figure = "jaccard",party = "all",color = c("red","blue"))
plotExportGGP(name = "figures/jaccardHeat",figsize = 3,ggp = T)

# getLRVisitIDPlot(file1 = "./csv_results/leftJsCookiesCountMedian.csv",
#                  file2 = "./csv_results/rightJsCookiesCountMedian.csv",
#                  xLab = "Cookies Count",
#                  yLab = "CDF",figure = "zscore",party = "all",color = c("red","blue"))
########Overlap with persona####
getLRVisitIDPlot(file1 = "./csv_results/leftOverlapPercent.csv",
                 file2 = "./csv_results/rightOverlapPercent.csv",
                 xLab = "Training Personas",
                 yLab = "Percent",figure = "box",party = "all",color = c("red","blue"))
plotExportGGP(name = "figures/overlapPercent",type = ".png",
              figsize = 1.5,ggp = T)

##Fox news and MSNBC plot, read from cookies counts
#topSiteCookies <- fileRead()
ind <- gsub("A|C|N","",topSiteCookies$ï..)
topSiteCookies$ï.. <- ind
uq <- unique(ind)
uq <- uq[-which(uq=="")]
topSiteCookies <- topSiteCookies[ind%in%uq,]
topSiteCookies <- melt(topSiteCookies)
colnames(topSiteCookies) <- c("id","var","value")
plotBarGGP2(topSiteCookies,varNames = unique(topSiteCookies$var),
            xLab = "Persona",yLab = "Cookies count",sort = T,label = "Website",
            color=c("blue","red"))

plotExportGGP(name = "./figures/countIds/foxnewsCookies",figsize = 2,type = ".png")

#######NMF Analysis######
library(NMF)
#https://cran.r-project.org/web/packages/NMF/vignettes/NMF-vignette.pdf

#we do NMF by breaking up categories of cookies- 6 types
#get all type NMF JSC, 50 runs, pass file names
#send file1 cookies from left and file2 cookies from right 
nmfResJSC <- getNMFEstim(file1 = file1,file2=file2,
                allType="../Telefonica/allJSDomainsTypeUpdated.csv",
                filterLoc=T)
#storing NMF for categories
nmfResJSC <- estimAll
#storing NMF for all
estim
#JSC should be 18 elements list
