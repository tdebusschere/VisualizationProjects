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
  titlePanel("Deposit | Withdraw"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      actionButton("appProbabilities", label="Calculate Probabilities"),
      sliderInput("CutoffTotal", "Cutoff Total", 0, 15, value = 1, pre = "#"),
      sliderInput("CutoffAmount", "Cutoff Amount", 0, 500000, value = 1, pre = "$"),
      sliderInput("CutoffDef", "Cutoff Defecit",-50000,0, value = -50, pre='$'),
      sliderInput("CutoffLRate", "Cutoff Loss_Rate",-2.0, 0, value = -1.5, pre='$'),
      actionButton("calculateCopula", label="Calculate Copula")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id='Savings',
                  tabPanel('Overview', dataTableOutput("soverview"),value = 1)
                  
                  
    )
  )
)
))



