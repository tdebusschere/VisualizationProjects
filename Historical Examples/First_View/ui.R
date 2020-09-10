#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(TSA)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Users per website"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'parameter', multiple = FALSE, label = "to Select",
                  choices = c('distinct_user','total_bets','avg_users','betamount','bet_player','avg_player','roi','profit')),
      conditionalPanel(condition="input.tpanel <= 2",
      selectInput(inputId = "website",multiple =FALSE,label = "website:",
                  choices = list()),
            selectInput(inputId = "gametypeid",multiple =FALSE,label = "Gametype:",
                  choices = list()),
            sliderInput(inputId = 'prediction_steps', label = 'prediction steps', min = 1, max = 10, step = 1, value = 5),
        actionButton("update",'Update!')
    ), conditionalPanel(condition='input.tpanel > 2',actionButton("runthrough",'Update!') )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id='tpanel',
      tabPanel('One by one',            
       plotOutput("GameDistribution"),
       plotOutput("hw"),
       plotOutput("Covariates"), value = 1
      ),
      tabPanel('Aggegrated', 
               plotOutput("Distribution"),
               plotOutput("Exponential"),
               plotOutput("Covariates_Gen"), value = 2),
      tabPanel('RunThrough',dataTableOutput('RunThrough2'), value = 3 )
    )
    
    
  )
  )
))
