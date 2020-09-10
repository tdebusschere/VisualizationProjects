library(shiny)
library(BH)
library(rCharts)
require(markdown)
require(data.table)
library(dplyr)
library(DT)
library(RODBC)
sqlstring <- odbcDriverConnect('driver={SQL Server};server=JG\\MSSQLSERVER2016;uid=DS.Jimmy;pwd=4wvb%ECX')
index <- sqlQuery(sqlstring,"select * from [GoogleAnalytics].[dbo].[GoogleAnalyticsViewingID] order by sn",
                  stringsAsFactors=F)
browser <- c('Chrome', 'Safari', 'Safari (in-app)','Android Webview', 'UC Browser', 'Android Browser', 'Internet Explorer',
             'Samsung Internet', 'Firefox', 'Edge', 'Maxthon', 'Opera')
shinyUI(
  navbarPage('GA_Browser',
             tabPanel('Overview',  
                      #runApp('C:\\GA\\shiny_test', launch.browser =FALSE,port = 1234, host = '10.80.29.124')
                      wellPanel(
                        checkboxGroupInput("webname","Choose web:",index$WebName),
                        actionLink("selectall","Select All")
                        ),
                      wellPanel(
                        checkboxGroupInput("Browser","Choose Browser:",browser),
                        actionLink("selectall_browser","Select All") ,
                        downloadButton("downloadData", "Download"))),
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   p(icon("table"), "Data"),dataTableOutput(outputId="dTable"),
                   wellPanel(
                     span("Date selected:",textOutput("n_webs")))
                  ),# end of "Data" tab panel
                 tabPanel(p(icon("table"), "Visualize the Data"),
                          h3('Users'),
                          showOutput("Users", "nvd3")
                 )
               )
             )
  )
)



