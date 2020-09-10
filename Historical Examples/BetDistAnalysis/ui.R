#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)




# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("games / website"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition="input.tpanel == 0",
                       uiOutput("games"),br(),
      actionButton("selectall", label="Select/Deselect all"), 
      selectInput(inputId="Cumuldist", label = "select variable", selected = c(), multiple = FALSE, 
                  choices = list("nbets","betamount","total_return")  )),
      conditionalPanel(condition="input.tpanel == 0 && input.Cumuldist =='total_return'", sliderInput(inputId ='roicoff', label = 'cutoff roi',
                                                                               min = -0.5, max=0.5, value=0, step=0.01)),
      
      conditionalPanel(condition="input.tpanel != 0", 
                  selectInput(inputId = "gametypeids", selected = 42, label = "sel gametypeids", multiple = FALSE  , choices = list()), 
                  uiOutput("websites"),br()),

      conditionalPanel(condition="input.tpanel == 4 | input.tpanel == 3 | input.tpanel == 5", 
          selectInput(inputId = "UserAtRisk", selected = 918226, label = "UserAtRisk", multiple = FALSE, choices = list())),
      conditionalPanel(condition="input.tpanel == 4",
          sliderInput(inputId = "BetAmount", label = "BetAmount User > ", min = 0.0, max = 1.0, 0, pre = "$"),
          sliderInput(inputId = "BetAmountG", label = "BetAmount Global > ", min = 0,max = 1.0, 0, pre = "$")
      ),
      conditionalPanel(condition="input.tpanel == 5",
            selectInput(inputId = "random_samples",multiple =FALSE,label = "Type:",
            choices = list("n5" = 5, "n10" = 10, "n20" = 20, "n50" = 50 , "n100" = 100, "n200" = 200, "n500" = 500, 
                           "n1000" = 1000, "n10000" = 10000, "selected user" = -1 )),
            selectInput(inputId = "NormDist",
                        multiple =FALSE,label = "Overlay Normal Distribution", choices = list("Limit", "Yes", "No")),
            selectInput(inputId = "m_cutoff", multiple = FALSE, label = "min cutoff", choices = list("Yes", "No") )
    ), 
    conditionalPanel(condition="input.tpanel == 6", 
            selectInput(inputId = "comparesite",multiple = FALSE, label = "compare to ..", choices = list()))),
    # Show a plot of the generated distribution
    mainPanel(

      tabsetPanel(id='tpanel',
      tabPanel("Game per site", dataTableOutput("table_games"), dataTableOutput("aggregated_games"), dataTableOutput("per_website"),
               value=0),
      tabPanel('Site Overview', dataTableOutput("myTable"),value = 1),
      tabPanel('UserDetails', dataTableOutput("UserAggregates"), value = 3),
      tabPanel('UserGraphs',conditionalPanel(condition = "input.UserAtRisk > 1", 
                       plotOutput("basicDensity"), plotOutput("userDensity"), tableOutput("selected_userstats")), value=4),
      tabPanel('ROI', tableOutput("selected_userstat"), plotOutput("sampledDataPlot"),tableOutput("oddstable"), 
               conditionalPanel(condition = "input.NormDist=='Yes", plotOutput("VisNorm")),value = 5 ),
      tabPanel('ROI Compare', plotOutput("cSite"), plotOutput("basiccDensity"), plotOutput("compoddstable"), 
               value = 6),
      tabPanel('BetAmount',plotOutput("betamount_dist"), plotOutput("betamount_fit_dist"), plotOutput("betamount_tail_dist")),
      tabPanel('BetFrequency',plotOutput("nbets_dist"), plotOutput("nbets_fit_dist"), plotOutput("nbets_tail_dist"))      )
  )
)))
