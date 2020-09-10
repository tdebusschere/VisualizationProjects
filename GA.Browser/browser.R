rm(list=ls())
setwd('C:\\GA')
library(devtools)
library(curl)
library(rga)
library(dplyr)
library(reshape2)
library(data.table)
library(RODBC)
rga.open(instance = "ga",
         client.id = "834281821508-t4nmvlpkqg1br1j32odgt8gng39o35pm.apps.googleusercontent.com",
         client.secret = "YKIgly5dMtBIWgwZB3lHfv3i", where="ga.rga")

#get the data from GA api
sqlstring <- odbcDriverConnect('driver={SQL Server};server=JG\\MSSQLSERVER2016;uid=DS.Jimmy;pwd=4wvb%ECX')
index <- sqlQuery(sqlstring,"select * from [GoogleAnalytics].[dbo].[GoogleAnalyticsViewingID] order by sn",stringsAsFactors=F)
index$Viewingid <- as.character(index$Viewingid)
index$WebStartDate <- as.Date(index$WebStartDate)
StartDate <- as.Date('2018-11-15')
EndDate <- Sys.Date()-1
for(i in 1:nrow(index)){
  if (StartDate < EndDate) {
    GAInsert <- ga$getData(index$Viewingid[i], batch = TRUE, StartDate, EndDate, walk=TRUE,
                           metrics = "ga:users,ga:newUsers,ga:sessions,ga:bounceRate,ga:pageviewsPerSession,ga:avgSessionDuration", 
                           dimensions = "ga:browser,ga:deviceCategory",
                           filters = "", segment = "", max=50000)
    if (i==1) {
      input <- data.frame(index$WebName[i],GAInsert)
    } else {
      input <- rbind(input,data.frame(index$WebName[i],GAInsert))
    }
  } 
}

write.csv(input, 'Ken_ga.csv',row.names = F)
