#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(RODBC)
library(shiny)
library(data.table)
library(fitdistrplus)
library(evir)
library(goftest)
library(VineCopula)
library(plyr)

options(stringsAsFactors=FALSE)
handle = odbcDriverConnect('driver=SQL Server;server=DESKTOP-UVDOJ3H\\MSSQLSERVER16;')
datafr = sqlQuery(handle,'select distinct membernr memberid, withdraws,withdraw_total,max_withdraw_once,deposit, deposit_total, 
                  max_deposit_once, (deposit_total - withdraw_total) defecit, ( deposit_total - withdraw_total) / 
case when deposit_total = 0.0 then 1.0 else deposit_total end leakage
                  from CasinoCash.dbo.userrecentprofile')

fp_withdraws <- fitdist(datafr$withdraws, "gpd",method='mge',gof='CvM',start=c(xi=0.7,mu=1.2, beta = 1))
fp_withdrawamount <- fitdist(datafr$withdraw_total, "gpd",method='mge',gof='CvM',start=c(xi=0.7,mu=1.2, beta = 1))
fp_difference <- fitdist(datafr$defecit,"cauchy")
fp_l_rate <-fitdist(datafr$leakage,"cauchy")
gofstat(fp_withdraws)
gofstat(fp_withdrawamount)
gofstat(fp_difference)
gofstat(fp_l_rate)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$soverview = renderDataTable({datafr})

  observeEvent(input$CutoffTotal,{
    output$soverview = renderDataTable({subset(datafr,withdraws > input$CutoffTotal || withdraw_total > input$CutoffAmount || 
                                            defecit < input$CutoffDef || leakage < input$CutoffTotal )})
  })
  
  observeEvent(input$CutoffAmount,{
    output$soverview = renderDataTable({subset(datafr,withdraws > input$CutoffTotal || withdraw_total > input$CutoffAmount || 
                                                 defecit < input$CutoffDef || leakage < input$CutoffTotal )})
    
  })

  observeEvent(input$CutoffDef,{
    output$soverview = renderDataTable({subset(datafr,withdraws > input$CutoffTotal || withdraw_total > input$CutoffAmount || 
                                                 defecit < input$CutoffDef || leakage < input$CutoffTotal )})    
  })
  observeEvent(input$CutoffLRate,{
    output$soverview = renderDataTable({subset(datafr,withdraws > input$CutoffTotal || withdraw_total > input$CutoffAmount || 
                                                 defecit < input$CutoffDef || leakage < input$CutoffTotal )})  })

  observeEvent(input$appProbabilities,{
     datafr$withdrawp = pgpd(datafr$withdraws, xi=fp_withdraws$estimate[1], mu = fp_withdraws$estimate[2], beta = fp_withdraws$estimate[3]  )
     datafr$withdrawamtp = pgpd(datafr$withdraw_total, xi=fp_withdrawamount$estimate[1], mu = fp_withdrawamount$estimate[2], beta = fp_withdrawamount$estimate[3]  )
     datafr$differencep = pcauchy(datafr$defecit, location=fp_difference$estimate[1], scale = fp_difference$estimate[2])
     datafr$leakagep = pcauchy(datafr$leakage, location=fp_l_rate$estimate[1], scale = fp_difference$estimate[2])
     output$soverview = renderDataTable({subset(datafr,withdraws > input$CutoffTotal || withdraw_total > input$CutoffAmount || 
                                                  defecit < input$CutoffDef || leakage < input$CutoffTotal )})  
  
  })
  
  
  
  observeEvent(input$calculateCopula,{
    copula = RVineStructureSelect(subset[,c('withdrawp','withdrawamtp','differencep','leakagep')],  
                                  type=0, selectioncrit ="BIC",indeptest = TRUE, treecrit = "BIC", cores = 4)
    subset = datafr[,c('withdrawp','withdrawamtp','differencep','leakagep')]
    subset[ subset < 0] = 0
    n = 10000
    sample = RVineSim(n, copula)
    reverse_sample = data.frame(replicate(4,rep(1,n))) 
    reverse_sample[,1]  = qgpd(sample[,1], xi=fp_withdraws$estimate[1], mu = fp_withdraws$estimate[2], beta = fp_withdraws$estimate[3]  )
    reverse_sample[,2]  = qgpd(sample[,2], xi=fp_withdrawamount$estimate[1], mu = fp_withdrawamount$estimate[2], beta = fp_withdrawamount$estimate[3]  )
    reverse_sample[,3]  = qcauchy(sample[,3], location=fp_difference$estimate[1], scale = fp_difference$estimate[2])
    reverse_sample[,4]  = qcauchy(sample[,4], location=fp_l_rate$estimate[1], scale = fp_l_rate$estimate[2])
    colnames(reverse_sample) = c('withdraws','withdraw_amount','difference','l_rate')
    
    zm =  function(testobject) { return(dim(reverse_sample[reverse_sample$withdraws <= testobject[1] &
                                        reverse_sample$withdraw_amount <= testobject[2] &
                                        reverse_sample$difference >= testobject[3] & 
                                        reverse_sample$l_rate >= testobject[4] 
                                                              ,])[1] / n)}
    km =  function(testobject) { return(dim(datafr[datafr$withdraws <= testobject[1] &
                                          datafr$withdraw_total <= testobject[2] &
                                          datafr$defecit >= testobject[3] & 
                                          datafr$leakage >= testobject[4] 
                                                           ,])[1] / n)}
    
    probs = apply(datafr[,c('withdraws','withdraw_total','defecit','leakage')], 1,zm)
    alt_probs = apply(datafr[,c('withdraws','withdraw_total','defecit','leakage')], 1,km)
    })
})


subset = datafr[,c('withdrawp','withdrawamtp','differencep','leakagep')]
subset[ subset < 0] = 0



