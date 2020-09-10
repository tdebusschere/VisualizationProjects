library(shiny)
library(BH)
library(rCharts)
require(markdown)
require(data.table)
library(dplyr)
library(DT)

shinyUI(
  navbarPage('GA_Sumup',
             tabPanel('Overview',  
                      #runApp('C:\\GA\\shiny_test', launch.browser =FALSE,port = 1234, host = '10.80.29.124')
                      sidebarPanel(
                        wellPanel(
                          selectInput("Path", "AllPagevsLandingPath", c('All','LandingPath'), selected = "All"),
                          selectInput("Web", "SearchWeb", c('/','/Lobby/RecommendGame','/Lobby/MGHtml'), selected = "/Lobby/RecommendGame"),
                          selectInput("GroupBy", "Group", c('UserType','Device'), selected = "UserType"),
                          selectInput("WebType", "WebType", c('All','New','Old'), selected = "All")),
                        wellPanel(
                          sliderInput("StartDate", "StartDate",
                                      as.Date('2018-07-01'), Sys.Date()-1, as.Date('2018-11-15'),timeFormat = "%Y-%m-%d"),
                          sliderInput("EndDate", "EndDate",
                                      as.Date('2018-07-01'), Sys.Date()-1, Sys.Date()-1,timeFormat = "%Y-%m-%d")),
                          downloadButton("downloadData", "Download"))),
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   p(icon("table"), "Data"),dataTableOutput(outputId="dTable"),
                   wellPanel(
                     span("Webs selected:",textOutput("n_webs")),
                     span(textOutput("sessionsorpageview"),textOutput("n_pageview")),
                     span("Total users:",textOutput("n_user")))),# end of "Data" tab panel
                 tabPanel(p(icon("table"), "Visualize the Data"),
                          h3(textOutput("caption")),#Sessions
                          showOutput("Pageviews", "nvd3"),#Sessions
                          h3('Users'),
                          showOutput("Users", "nvd3")
                 )
               )
             )
  )
)
