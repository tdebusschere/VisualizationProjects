rm(list=ls())
library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)
library(DT)
library(rCharts)
library(RODBC)
# runApp('C:\\GA\\browser', launch.browser =FALSE,port = 8888, host = '10.80.29.124')
setwd('C:\\GA')
sqlstring <- odbcDriverConnect('driver={SQL Server};server=JG\\MSSQLSERVER2016;uid=DS.Jimmy;pwd=4wvb%ECX')
index <- sqlQuery(sqlstring,"select * from [GoogleAnalytics].[dbo].[GoogleAnalyticsViewingID] order by sn",
                  stringsAsFactors=F)
GABrowser <- fread('Ken_ga.csv',header = T)
colnames(GABrowser)[1] <- 'Webname'
browser <- c('Chrome', 'Safari', 'Safari (in-app)','Android Webview', 'UC Browser', 'Android Browser', 'Internet Explorer',
             'Samsung Internet', 'Firefox', 'Edge', 'Maxthon', 'Opera')

function(input, output,session) {
  observe({
    if(input$selectall == 0) {return(NULL) 
    } else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"webname","Choose web:", choices = index$WebName)
    } else {
      updateCheckboxGroupInput(session,"webname","Choose web:", choices = index$WebName, selected = index$WebName)
    }
  })
  observe({
    if(input$selectall_browser == 0) {return(NULL) 
     } else if (input$selectall_browser%%2 == 0) {
      updateCheckboxGroupInput(session,"Browser","Choose Browser:", choices = browser)
    } else {
      updateCheckboxGroupInput(session,"Browser","Choose Browser:", choices = browser, selected = browser)
    }
  })  
  observeEvent(
    c(input$webname, input$Browser),
    { webchoice <- GABrowser[(GABrowser$Webname %in% input$webname)& (GABrowser$browser %in% input$Browser), ]#
      #print(input$selectall_browser)
      #webchoice <- webchoice[webchoice$browser %in% input$Browser, ]
      #print(head(webchoice))
      groupby <<- data.frame(webchoice %>% 
                  group_by(browser,deviceCategory) %>% 
                  summarise(users = sum(users), newUsers = sum(newUsers), sessions = sum(sessions), 
                  bounceRate = mean(bounceRate) , pageviewsPerSession = mean(pageviewsPerSession), 
                  avgSessionDuration.min = mean(avgSessionDuration)))
      groupby.visualize <<- data.frame(webchoice %>% 
                               group_by(browser) %>% 
                               summarise(users = sum(users)))
      groupby.visualize <- groupby.visualize[order(groupby.visualize$users,decreasing = T),]
      string_min <- groupby$avgSessionDuration.min%/%60
      string_sec <- as.character(round(groupby$avgSessionDuration.min%%60,0))
      if (nrow(groupby)!=0){
        for (j in 1:length(string_sec)) {
            if (nchar(string_sec[j]) == 1) {
              string_sec[j] <- paste('0', string_sec[j],sep = '')
            } 
            if (nchar(string_min[j]) == 1) {
              string_min[j] <- paste('0', string_min[j],sep = '')
            }
        }
      }
      groupby$avgSessionDuration.min <- paste(string_min,string_sec,sep = ':')
      groupby <- groupby[order(groupby$users, decreasing = T), ]
      output$dTable <- renderDataTable({groupby})
    })
  output$Users <- renderChart({
    p6 <- nPlot(users ~ browser, data = as.data.frame(groupby.visualize[order(groupby.visualize$users,decreasing = T),]), 
                type = 'multiBarChart', dom = 'Users', width = 600)
    #p6$yAxis(axisLabel = "Users", width = 40)
    p6$xAxis(axisLabel = 'browser')
    p6$chart(forceY = c(0, 1.2 * max(groupby.visualize$users)))
    p6$chart(color = c('brown','brown', 'blue', '#594c26', 'green'))
    return(p6)
  })
  output$n_webs <- renderText({paste('2018-11-15~',Sys.Date()-1,sep = '')})
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('GA_browser',
            input$Path,
            input$GroupBy,
            substr(as.character(Sys.Date()),start=1,stop=4),
            substr(as.character(Sys.Date()),start=6,stop=7),
            substr(as.character(Sys.Date()),start=9,stop=10),
            '.csv',
            sep = '')
    },
    content = function(file) {
      write.csv(groupby, file, row.names = FALSE)
    }
  )
  
}