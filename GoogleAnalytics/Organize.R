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
EndDate <- Sys.Date()-1
for(i in 1:nrow(index)){
  StartDate <- sqlQuery(sqlstring,
                        paste("Select max(Date) as Date from [GoogleAnalytics].[dbo].[GoogleAnalyticsDailyRecord] where web='",
                        index$WebName[i],"'",sep=''),
                        stringsAsFactors=F)#class(StartDate)=data.frame
  StartDate <- as.Date(StartDate[[1]])
  if (is.na(StartDate)) { StartDate <- index$WebStartDate[i]-1 }
  if (StartDate < EndDate) {
      GAInsert <- ga$getData(index$Viewingid[i], batch = TRUE, StartDate+1, EndDate, walk=TRUE,
                             metrics = "ga:sessions,ga:users,ga:pageviews", 
                             dimensions = "ga:pagePath,ga:landingPagePath,ga:userType,ga:date,ga:deviceCategory",
                             filters = "", segment = "", max=50000)
  } else {next}
  #one time insert 1000 observations to SQL table
  times <- ceiling(nrow(GAInsert)/1000)
  for (j in 1:times) {
    if (j!=times) {
      cursor <- GAInsert[c((j*1000-999):(j*1000)), ]
    } else {
      cursor <- GAInsert[c((j*1000-999):nrow(GAInsert)), ]
    }
    sqlString <- paste("insert into [GoogleAnalytics].[dbo].[GoogleAnalyticsDailyRecord] values")
    body <- paste(paste("('", index$WebName[i], "',", sep=''),
                  paste("'", apply(cursor[, c(1:5)], 1, paste , sep="", collapse="','"), "'", sep=""),
                  paste(",", cursor[, 6], ",", cursor[, 7], ",", cursor[, 8], ")", sep="")
                  , sep="", collapse=",")
    FullString <- paste(sqlString,body ,sep="" ,collapse="")
    sqlQuery(sqlstring, FullString)
  }
  print(i)
}
print("insert SQL done")


for(i in 1:nrow(index)){
  GAwebUntilNow <- sqlQuery(sqlstring,
    paste("Select [PagePath],[LandingPagePath],[UserType],[Date],[deviceCategory],[sessions],[Users],[pageviews] 
          from [GoogleAnalytics].[dbo].[GoogleAnalyticsDailyRecord] where web='",index$WebName[i],"'",sep=''),
          stringsAsFactors=FALSE)
  colnames(GAwebUntilNow) <- c("pagePath", "landingPagePath", "userType", "date", 
                               "deviceCategory", "sessions", "users","pageviews")
  GAwebUntilNow$date <- as.Date(GAwebUntilNow$date)
  GAAggregatedLandingPage <- GAwebUntilNow %>%
                            group_by(landingPagePath)%>% 
                            summarise(userType='Allusers',sessions=sum(sessions),users=sum(users))
  GAAggregatedLandingPage <- GAAggregatedLandingPage[order(GAAggregatedLandingPage$users,decreasing = T), ]
  GAAggregatedLandingPage <- GAAggregatedLandingPage[1:100, ]
  Top100LandingPage <- GAwebUntilNow$landingPagePath %in% GAAggregatedLandingPage$landingPagePath
  ChosenLandingPage <- GAwebUntilNow[Top100LandingPage, ]
  
  
  GAAggregatedAllPage <- GAwebUntilNow %>% 
                        group_by(pagePath) %>% 
                        summarise(userType='Allusers',sessions=sum(sessions),users=sum(users))
  GAAggregatedAllPage <- GAAggregatedAllPage[order(GAAggregatedAllPage$users,decreasing = T),]
  GAAggregatedAllPage <- GAAggregatedAllPage[1:100,]
  Top100AllPage <- GAwebUntilNow$pagePath %in% GAAggregatedAllPage$pagePath
  ChosenAllPage <- GAwebUntilNow[Top100AllPage, ]
  
  
  #Structure
  StartDate <- index$WebStartDate[i]
  EndDate <- Sys.Date()-1
  Process <- StartDate
  date <- c()
  landingPagePath <- c()
  userType <- c()
  allpage <- c()
  device=c()
  while (Process <= EndDate) {
    device <- c(device,rep(c("mobile", "tablet", "desktop"),100))
    userType <- c(userType,rep(c('Allusers','Returning Visitor','New Visitor'),100))
    date <- c(date,rep(as.character(Process),300))
    landingPagePath <- c(landingPagePath,rep(GAAggregatedLandingPage$landingPagePath,3))
    allpage <- c(allpage,rep(GAAggregatedAllPage$pagePath,3))
    Process <- Process+1
  }
  date <- as.Date(date)
  StructureLandingPage <- data.frame(date,landingPagePath,userType,stringsAsFactors = F)
  StructureAllPage <- data.frame(date,pagePath=allpage,userType,stringsAsFactors = F)
  Str.landing.device <- rbind(StructureLandingPage[StructureLandingPage$userType=='Allusers',],
                              data.frame(date,landingPagePath,userType=device,stringsAsFactors = F))
  colnames(Str.landing.device)[3] <- 'deviceCategory'
  Str.all.device <- rbind(StructureAllPage[StructureAllPage$userType=='Allusers',],
                          data.frame(date,pagePath=allpage,userType=device,stringsAsFactors = F))
  colnames(Str.all.device)[3] <- 'deviceCategory'
  ORDER <- function(input){
    input <- input[order(input$date),]
  }  
  StructureLandingPage <- ORDER(StructureLandingPage)
  StructureAllPage <- ORDER(StructureAllPage)
  Str.landing.device <- ORDER(Str.landing.device)
  Str.all.device <- ORDER(Str.all.device)
  
  #Landing path
  GALandingAllUsers <- as.data.frame(ChosenLandingPage %>% 
              group_by(landingPagePath,date)%>% 
              summarise(userType='Allusers',sessions = sum(sessions),users = sum(users)))
  GALandingAllUsers.clone <- GALandingAllUsers
  colnames(GALandingAllUsers.clone)[3] <- 'deviceCategory'
  GAGroupByLandingPageUsertypeDate <- as.data.frame(ChosenLandingPage%>% 
                                      group_by(landingPagePath,date,userType)%>%
                                      summarise(sessions = sum(sessions),users = sum(users)))
  GAGroupByLandingPageDeviceDate <- as.data.frame(ChosenLandingPage%>% 
                                      group_by(landingPagePath,date,deviceCategory)%>%
                                      summarise(sessions = sum(sessions),users = sum(users)))
  GAGroupByLandingPageUsertypeDate <- rbind(GAGroupByLandingPageUsertypeDate,GALandingAllUsers)
  GAGroupByLandingPageDeviceDate <- rbind(GAGroupByLandingPageDeviceDate,GALandingAllUsers.clone)
  GALandingPage_tmp <- merge(StructureLandingPage,
                            GAGroupByLandingPageUsertypeDate,
                            by=c('date','landingPagePath','userType'),
                            all.x = TRUE,all.y = FALSE)
  GALandingPage_device <- merge(Str.landing.device,
                                GAGroupByLandingPageDeviceDate,
                             by=c('date','landingPagePath','deviceCategory'),
                             all.x = TRUE,all.y = FALSE)  
  GALandingPage_tmp[is.na(GALandingPage_tmp)] <- 0
  GALandingPage_device[is.na(GALandingPage_device)] <- 0
  GALandingPage_tmp <- data.frame(web = index$WebName[i],GALandingPage_tmp)
  GALandingPage_device <- data.frame(web = index$WebName[i],GALandingPage_device)
  colnames(GALandingPage_device)[4] <- 'device'
  if (i==1){ 
    GALandingPage <- GALandingPage_tmp 
    GALandingDevice <- GALandingPage_device
  } else { 
    GALandingPage <- rbind(GALandingPage,GALandingPage_tmp) 
    GALandingDevice <- rbind(GALandingDevice,GALandingPage_device)
  } 
  #all path
  GAAllPageAllUsers <- as.data.frame(ChosenAllPage %>% 
                      group_by(pagePath,date) %>% 
                      summarise(userType='Allusers', pageviews = sum(pageviews), users = sum(users)))
  GAAllPageAllUsers.clone <- GAAllPageAllUsers
  colnames(GAAllPageAllUsers.clone)[3] <- 'deviceCategory'
  GAGroupByAllPageUsertypeDate <- as.data.frame( ChosenAllPage %>% 
                                group_by(pagePath,date,userType)%>%
                                summarise(pageviews = sum(pageviews), users = sum(users)))
  GAGroupByAllPageDeviceDate <- as.data.frame( ChosenAllPage %>% 
                                group_by(pagePath,date,deviceCategory)%>%
                                summarise(pageviews = sum(pageviews), users = sum(users)))  
  GAGroupByAllPageUsertypeDate <- rbind(GAGroupByAllPageUsertypeDate,GAAllPageAllUsers)
  GAGroupByAllPageDeviceDate <- rbind(GAGroupByAllPageDeviceDate,GAAllPageAllUsers.clone)
  GAAllPage_tmp <- merge(StructureAllPage,
                        GAGroupByAllPageUsertypeDate,
                        by=c('date','pagePath','userType'),
                        all.x = TRUE,all.y = FALSE)
  GAAllPage_device <- merge(Str.all.device,
                            GAGroupByAllPageDeviceDate,
                         by=c('date','pagePath','deviceCategory'),
                         all.x = TRUE,all.y = FALSE)  
  GAAllPage_tmp[is.na(GAAllPage_tmp)] <- 0
  GAAllPage_device[is.na(GAAllPage_device)] <- 0
  GAAllPage_tmp <- data.frame(web=index$WebName[i],GAAllPage_tmp)
  GAAllPage_device <- data.frame(web=index$WebName[i],GAAllPage_device)
  colnames(GAAllPage_device)[4] <- 'device'
  if (i==1) {
    GAAllPage <- GAAllPage_tmp
    GAAllDevice<- GAAllPage_device
  } else {
    GAAllPage <- rbind(GAAllPage,GAAllPage_tmp)
    GAAllDevice <- rbind(GAAllDevice,GAAllPage_device)
  }
  print(i)
}


#Delete all the output before
# sqlQuery(sqlstring, "Delete from [GoogleAnalytics].[dbo].[GoogleAnalyticsLandingPathResult]")
# sqlQuery(sqlstring, "Delete from [GoogleAnalytics].[dbo].[GoogleAnalyticsAllPageResult]")
# 
# #output insert into table
# InsertResult=function(output){
#   times=ceiling(nrow(output)/1000)
#   for (j in 1:times) {
#     if(j!=times){cursor <- output[c((j*1000-999):(j*1000)),]}
#     else{cursor <- output[c((j*1000-999):nrow(output)),]}
#     if (colnames(output)[3]=="landingPagePath") {
#       sqlString <- paste("insert into [GoogleAnalytics].[dbo].[GoogleAnalyticsLandingPathResult] values")
#     } else if (colnames(output)[3]=="pagePath") {
#       sqlString <- paste("insert into [GoogleAnalytics].[dbo].[GoogleAnalyticsAllPageResult] values")
#     }
#     body <- paste(paste("('",apply(cursor[,c(1:4)],1,paste,sep="",collapse="','"),"'",sep=""),
#                  paste(",",cursor[,5],",",cursor[,6],")",sep="")
#                  ,sep="",collapse=",")
#     FullString <- paste(sqlString,body,sep="",collapse="")
#     sqlQuery(sqlstring, FullString)
#   }
# }
# InsertResult(GALandingPage)
# InsertResult(GAAllPage)

#output csv
write.csv(GALandingPage,"ga_LandingPageUsertype.csv",row.names = F)
write.csv(GAAllPage,"ga_AllpathUsertype.csv",row.names = F)
write.csv(GALandingDevice,"ga_LandingPagedevice.csv",row.names = F)
write.csv(GAAllDevice,"ga_AllpathDevice.csv",row.names = F)



