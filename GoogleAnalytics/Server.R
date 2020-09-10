rm(list=ls())
library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)
library(DT)
library(rCharts)
library(RODBC)
#runApp('C:\\GA\\shiny_test', launch.browser =FALSE,port = 1234, host = '10.80.29.124')
setwd('C:\\GA')
sqlstring <- odbcDriverConnect('driver={SQL Server};server=JG\\MSSQLSERVER2016;uid=DS.Jimmy;pwd=4wvb%ECX')
index <- sqlQuery(sqlstring,"select * from [GoogleAnalytics].[dbo].[GoogleAnalyticsViewingID] order by sn",
                  stringsAsFactors=F)
index$WebStartDate <- as.Date(index$WebStartDate)
OldList <- index[index$WebStartDate== '2018-07-01', 'WebName']#2018-07-01~now
NewList <- index[index$WebStartDate== '2018-11-15', 'WebName']#2018-11-15~now
GALandingPage <- fread('ga_LandingPageUsertype.csv',header = T)
#GALandingPage$date <- as.Date(GALandingPage$date)
GAAllPage <- fread('ga_AllpathUsertype.csv',header = T)
#GAAllPage$date <- as.Date(GAAllPage$date)
GALandingDevice <- fread('ga_LandingPagedevice.csv',header = T)
#GALandingDevice$date <- as.Date(GALandingDevice$date)
GAAllDevice <- fread('ga_AllpathDevice.csv',header = T)
#GAAllDevice$date <- as.Date(GAAllDevice$date)


function(input, output,session) {
  #test2 = data.frame()
  #makeReactiveBinding('test2')
  observeEvent(c(input$Path,input$Web),{
    if(input$Path=='All'){
      updateSelectInput(session,'Web',choices =unique(GAAllPage$pagePath), selected = input$Web)
    }else{
      updateSelectInput(session,'Web',choices = unique(GALandingPage$landingPagePath), selected = input$Web)
    }
  })
  observeEvent(
    c(input$Path, input$GroupBy, input$StartDate, input$EndDate, input$WebType, input$Web),
    { 
      #variable = 'landingPagePath'
      if(input$Path == 'All' & input$GroupBy == 'UserType'){
        userchoose <- GAAllPage
        } else if (input$Path == 'All' & input$GroupBy == 'Device'){
        userchoose <- GAAllDevice 
        } else if (input$Path == 'LandingPath' & input$GroupBy == 'UserType'){
        userchoose <- GALandingPage 
        } else if (input$Path == 'LandingPath' & input$GroupBy == 'Device'){
        userchoose <- GALandingDevice
        }
      #print(variable)
      if(input$WebType == 'All'){cursor <- userchoose
      } else if (input$WebType == 'New'){cursor <- userchoose[userchoose$web %in% NewList, ]
      } else if (input$WebType == 'Old'){cursor <- userchoose[userchoose$web %in% OldList, ]}
      colnames(cursor)[3] <- 'pagePath'
      #colnames(cursor)[5] <- 'pageviews'
      cursorchosebyuser <- cursor[pagePath == input$Web & date >= input$StartDate & date <= input$EndDate]

      #ken ask to export
      if (input$Path == 'All' & input$GroupBy == 'UserType') {
        Export_tmp <- cursor[userType == 'Allusers' & date >= input$StartDate & date <= input$EndDate]
        Export <<- Export_tmp %>% group_by(pagePath) %>% summarise(pageviews = sum(pageviews),users = sum(users))
        Export <<- Export[order(Export$pageviews,decreasing = TRUE),]
      } else if (input$Path == 'All' & input$GroupBy == 'Device'){
        Export_tmp <- cursor[device == 'Allusers' & date >= input$StartDate & date <= input$EndDate]
        Export <<- Export_tmp %>% group_by(pagePath) %>% summarise(pageviews = sum(pageviews),users = sum(users))
        Export <<- Export[order(Export$pageviews,decreasing = TRUE),]
      } else if (input$Path == 'LandingPath' & input$GroupBy == 'Device'){
        Export_tmp <- cursor[userType == 'Allusers' & date >= input$StartDate & date <= input$EndDate]
        Export <<- Export_tmp %>% group_by(pagePath) %>% summarise(sessions = sum(sessions),users = sum(users))
        Export <<- Export[order(Export$sessions,decreasing = TRUE),]        
      } else if (input$Path == 'LandingPath' & input$GroupBy == 'Device'){
        Export_tmp <- cursor[device == 'Allusers' & date >= input$StartDate & date <= input$EndDate]
        Export <<- Export_tmp %>% group_by(pagePath) %>% summarise(sessions = sum(sessions),users = sum(users))        
      } 
      #print(head(cursorchosebyuser))
      if (input$Path=='All' & input$GroupBy=='UserType') {
        groupby <<- cursorchosebyuser %>% group_by(date,userType) %>% summarise(pageviews = sum(pageviews),users = sum(users))
      } else if (input$Path == 'All' & input$GroupBy == 'Device'){
        groupby <<- cursorchosebyuser %>% group_by(date,device) %>% summarise(pageviews = sum(pageviews),users = sum(users))        
      } else if (input$Path == 'LandingPath' & input$GroupBy == 'UserType'){
        groupby <<- cursorchosebyuser %>% group_by(date,userType) %>% summarise(sessions = sum(sessions),users = sum(users))        
      } else if (input$Path == 'LandingPath' & input$GroupBy == 'Device'){
        groupby <<- cursorchosebyuser %>% group_by(date,device) %>% summarise(sessions = sum(sessions),users = sum(users))        
      }
      groupby$date <- as.Date(groupby$date)
      print(head(groupby))
      output$dTable <- renderDataTable({groupby})
      if(input$Path=='All'){
        output$caption=renderText({'Pageviews'})
        output$Pageviews <- renderChart({
          #groupby$date <- as.Date(groupby$date)
          p6 <- nPlot(pageviews ~ date, group = colnames(groupby)[2], data = as.data.frame(groupby), 
                      type = 'lineChart', dom = 'Pageviews', width = 600)
          p6$xAxis(axisLabel = 'Date')
          p6$xAxis(tickFormat =   "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d*1000*3600*24));}!#"
                     , rotateLabels = -90)
          #p6$yAxis(axisLabel = "Sessions", width = 40)
          p6$chart(forceY = c(0, 1.2 * max(groupby$pageviews)))
          p6$chart(color = c('brown', 'blue', '#594c26', 'green'))
          return(p6)
        })
      } else {
        output$caption=renderText({'Sessions'})
        output$Pageviews <- renderChart({
          #groupby$date <- as.Date(groupby$date)
          p6 <- nPlot(sessions ~ date, group = colnames(groupby)[2], data = as.data.frame(groupby), 
                      type = 'lineChart', dom = 'Pageviews', width = 600 )
          p6$xAxis(axisLabel = 'Date')
          p6$xAxis(tickFormat = "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d*1000*3600*24));}!#"
                     , rotateLabels = -90)
          #p6$yAxis(axisLabel = "Sessions", width = 40)
          p6$chart(forceY = c(0, 1.2 * max(groupby$sessions)))
          p6$chart(color = c('brown', 'blue', '#594c26', 'green'))
          return(p6)
        })      
      }
      output$Users <- renderChart({
        p6 <- nPlot(users ~ date, group = colnames(groupby)[2], data = as.data.frame(groupby), 
                    type = 'lineChart', dom = 'Users', width = 600)
        #p6$yAxis(axisLabel = "Users", width = 40)
        p6$xAxis(axisLabel = 'Date')
        p6$xAxis(tickFormat =   "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d*1000*3600*24));}!#"
                   ,rotateLabels = -90)
        p6$chart(forceY = c(0, 1.2 * max(groupby$users)))
        p6$chart(color = c('brown', 'blue', '#594c26', 'green'))
        return(p6)
      })
      output$downloadData <- downloadHandler(
        filename = function() {
          paste('GA_',
                input$Path,
                input$GroupBy,
                substr(as.character(Sys.Date()),start=1,stop=4),
                substr(as.character(Sys.Date()),start=6,stop=7),
                substr(as.character(Sys.Date()),start=9,stop=10),
                '.csv',
                sep = '')
        },
        content = function(file) {
          write.csv(Export, file, row.names = FALSE)
        }
      )
      output$n_webs <- renderText({ifelse(input$WebType=='All',paste(c(NewList,OldList),collapse = ','),
                                  ifelse(input$WebType=='New',paste(NewList,collapse = ','),paste(OldList,collapse = ',')))})
      output$sessionsorpageview <- renderText({paste('Total ',colnames(groupby)[3],':',sep='')})
      output$n_pageview <- renderText({sum(groupby[groupby[,2]=='Allusers',3])})
      output$n_user <- renderText({sum(groupby[groupby[,2]=='Allusers','users'])})
    })
}