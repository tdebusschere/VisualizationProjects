#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(RODBC)
library(data.table)
library(shiny)
library(forecast)
library(nnetpredint)
library(keras)

options(stringsAsFactors=FALSE)

handle = odbcDriverConnect('driver=SQL Server;server=DESKTOP-UVDOJ3H\\MSSQLSERVER16;')

#'distinct_user','total_bets','avg_users','betamount','bet_player','avg_bet','roi','profit'



selections = data.table(sqlQuery(handle,paste("
select  
  count(distinct memberid) distinct_user,
  sum(total_bets) total_bets,
  rand() avg_users, 
  sum(total_bet) betamount,
  convert(float,sum(total_bet)) / case when convert(float,count(distinct memberid)) = 0 then 1.0 else convert(float,count(distinct memberid)) end bet_player ,
  convert(float,sum(total_bet)) / case when convert(float,sum(total_bets)) =0 then 1.0 else convert(float,sum(total_bets)) end  avg_player,
  convert(float,sum(total_return))  / case when convert(float,sum(total_bet))=0 then 1.0 else convert(float,sum(total_bet)) end  roi,
  sum(total_return) profit,
  gametypeid,wsite,calculate_date 
  from CasinoCash.dbo.memberid_gametypeid 
  group by gametypeid,wsite,calculate_date 
  order by wsite, gametypeid, calculate_date", sep=''), stringsAsFactors = FALSE)
)

selgroup = selections[,list(
                            distinct_user = sum(distinct_user),
                            total_bets = sum(total_bets), 
                            betamount=sum(betamount) ,
                            avg_users = 0,
                            bet_player = sum(bet_player * total_bets) / sum(total_bets),
                            avg_player = sum(avg_player * total_bets) / sum(total_bets),
                            roi = sum(roi*total_bets) / sum(total_bets),
                            profit = sum(profit)
                        ), 
                        by=list(wsite, calculate_date)]

gamegroup = selections[,list(
                          distinct_user = sum(distinct_user),
                          total_bets = sum(total_bets), 
                          betamount=sum(betamount) ,
                          avg_users = 0,
                          bet_player = sum(bet_player * total_bets) / sum(total_bets),
                          avg_player = sum(avg_player * total_bets) / sum(total_bets),
                          roi = sum(roi*total_bets) / sum(total_bets),
                          profit = sum(profit)  
                      ), 
                      by=list(gametypeid, calculate_date)]
print(gamegroup)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

  observe(selections,{
    updateSelectInput(session, "website", choices = sort(unique(selections$wsite)))  
    observeEvent(input$website,
                 {
                   updateSelectInput(session, "gametypeid", choices = sort(unique(selections[ wsite == input$website,gametypeid]))) 
                 })
  })
      
  observeEvent(input$update,{
    selectionsubset  = subset(selections, wsite == input$website & gametypeid == input$gametypeid)
    globalizedsubset = subset(gamegroup, gametypeid == input$gametypeid )
    
   dates = data.frame(sort(unique(selections$calculate_date)))
    colnames(dates) = c('dates')
    selectionsubset  = merge.data.frame(dates,selectionsubset, by.x = c('dates'), by.y = c('calculate_date'), all.x=TRUE )
    globalizedsubset = merge.data.frame(dates,globalizedsubset, by.x = c('dates'), by.y = c('calculate_date'), all.x=TRUE )
    globalizedsubset[is.na(globalizedsubset)] = 0.0
    
    selectionsubset[is.na(selectionsubset)] = 0.0
    if (( dim(selectionsubset)[1]< 1) || ( dim(dates)[1] < 15 + input$prediction_steps )) {
      print("not enough data")
      return(0)
    }
    print ( input$website )
    print ( input$gametypeid )
    print ( input$parameter )
    #print("Combination")
    ##specified: arima||exponentially smoothed
    #[1] "10.80.17.190_CasinoCash.BF001.H"   [1] "548"    [1] "avg_player"
    print( seq(1,dim(dates)[1]-input$prediction_steps) )
    print( selectionsubset[,c(input$parameter)] )
    
    myts = ts(selectionsubset[seq(1,dim(dates)[1]-input$prediction_steps),c(input$parameter)], 
              start = c(1, 0), frequency = 7 )
    myts2 = ts(as.data.frame(selectionsubset)[seq(1,dim(dates)[1]),input$parameter], start = c(1,0),
               frequency = 7 )
    
    model = auto.arima(myts, d=NA, D=NA, max.p=5, max.q=5, max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                       start.p=2, start.q=2, start.P=1, start.Q=1,  stationary=FALSE, seasonal=TRUE)
    fc_reg = forecast(model, h = input$prediction_steps)
    
    xmm = length(myts2)
    consensus = fc_reg$mean 

    
    scraper =  myts2[ c((xmm - input$prediction_steps + 1) : xmm) ] 
    

    if (sum( scraper  >  fc_reg$upper[,2] ) > 0)
    {
      consensus[ scraper >  fc_reg$upper[,2] ] = scraper[ scraper >  fc_reg$upper[,2] ] 
    } else if( sum(consensus[ scraper <  fc_reg$lower[,2]]   ) > 0)
    {
      consensus[ scraper <  fc_reg$lower[,2] ] = scraper [ scraper <  fc_reg$lower[,2] ] 
    }

    output$GameDistribution = renderPlot( 
      {
        plot(fc_reg) 
        points(myts2, col=c('green'))
        points(consensus, col = c('red'))
      } 
    )

        
    hw <- ets(myts,model = 'ZZZZ', damped = NULL, allow.multiplicative.trend = TRUE )
    hw_fc <- forecast(hw, h = input$prediction_steps, prediction.interval = T, level = 0.95)
    consensus_hw = hw_fc$mean 
    if (sum( scraper >  hw_fc$upper ) > 0)
    {
      consensus_hw[ scraper >  hw_fc$upper ] = scraper[scraper >  hw_fc$upper ] 
    } else if( sum(scraper <  hw_fc$lower  ))
    {
      consensus_hw[ scraper <  hw_fc$lower ] = scraper [ scraper <  hw_fc$lower ] 
    }

    output$hw = renderPlot(
      { 
        plot(hw_fc) 
        points(myts2, col = c('green'))
        points(consensus_hw, col = c('red'))
      }
    )
    #print( accuracy(hw_fc))
    steps = input$prediction_steps - 1
    print(names(as.data.frame(selectionsubset)))
    xreg = as.data.frame(selectionsubset)[,c('distinct_user','total_bets','avg_users','betamount','bet_player',
                                             'avg_player','roi','profit')]
    xreg =rbind(c(0,0,0), xreg)
    consensus_aa = fc_reg$mean
    model_up = fc_reg
    tryCatch({
    model_cov = auto.arima(myts, d=NA, D=NA, max.p=5, max.q=5, max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                           start.p=2, start.q=2, start.P=1, start.Q=1,  stationary=FALSE, seasonal=TRUE, 
                           xreg = as.matrix(xreg[seq(1,dim(xreg)[1]-input$prediction_steps - 1) ,]))
    model_up = forecast(model_cov,h = steps,  xreg = as.matrix(xreg[seq(dim(xreg)[1]-steps -
                            1,dim(xreg)[1] - 1),]),  bootstrap = TRUE)
    consensus_aa = model_up$mean
    
    #print("start consensus:")
    #print(consensus_aa)
    #print(model_up$mean)
    if ( sum( scraper >  model_up$upper[,2] ) > 0 )
    {
      consensus_aa[ scraper >  model_up$upper[,2] ] = scraper [scraper >  model_up$upper[,2]  ] 
    } else if( sum(scraper <  model_up$lower[,2]  ) > 0)
    {
      consensus_aa[ scraper <  model_up$lower[,2] ] = scraper [scraper <  model_up$lower[,2] ] 
    }    
    #print(consensus_aa)
    #print("consensus:")
    
    #print(consensus_aa)
    #print("end consensus")

    print(accuracy(model_cov))
    
    }, warning = function(w)
    {
      model_cov = auto.arima(myts, d=NA, D=NA, max.p=5, max.q=5, max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                             start.p=2, start.q=2, start.P=1, start.Q=1,  stationary=FALSE, seasonal=TRUE)
      model_up = forecast(model_cov, h = input$prediction_steps)
      
      xmm = length(myts2)
      consensus_aa = fc_reg$mean 
      
      scraper =  myts2[ c((xmm - input$prediction_steps + 1) : xmm) ] 
      if (sum( scraper  >  model_up$upper[,2] ) > 0)
      {
        consensus_aa[ scraper >  model_up$upper[,2] ] = scraper[ scraper >  model_up$upper[,2] ] 
      } else if( sum(consensus_aa[ scraper <  model_up$lower[,2]]   ) > 0)
      {
        consensus_aa[ scraper <  model_up$lower[,2] ] = scraper [ scraper <  model_up$lower[,2] ] 
      }
    },
    
    error = function(e) 
      {
      model_cov = auto.arima(myts, d=NA, D=NA, max.p=5, max.q=5, max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                         start.p=2, start.q=2, start.P=1, start.Q=1,  stationary=FALSE, seasonal=TRUE)
      model_up = forecast(model_cov, h = input$prediction_steps)
      
      xmm = length(myts2)
      consensus_aa = fc_reg$mean 
      
      scraper =  myts2[ c((xmm - input$prediction_steps + 1) : xmm) ] 
      if (sum( scraper  >  model_up$upper[,2] ) > 0)
      {
        consensus_aa[ scraper >  model_up$upper[,2] ] = scraper[ scraper >  model_up$upper[,2] ] 
      } else if( sum(consensus_aa[ scraper <  model_up$lower[,2]]   ) > 0)
      {
        consensus_aa[ scraper <  model_up$lower[,2] ] = scraper [ scraper <  model_up$lower[,2] ] 
      }
      
      })
    print(model_up)
    
    output$Covariates = renderPlot({
      plot(model_up) 
      points( myts2, col=c('green'))
      points(consensus_aa, col=c('red'))
    })
    
    print("Generalized:")
    print(globalizedsubset)
    print("Alles is het zowat")
    
    ###generalized per ws: arima || exponentially smoothed
    #print("Generalized")
    myts3 = ts(as.data.frame(globalizedsubset)[seq(1,dim(dates)[1]-input$prediction_steps),
                                               input$parameter], start = c(1, 0 ), frequency = 7 )
    myts4 = ts(as.data.frame(globalizedsubset)[seq(1,dim(dates)[1]),input$parameter], start = c(1,0),
               frequency = 7 )

    print( myts3 )
    print( myts4 )
    model_gen = auto.arima(myts3, d=NA, D=NA, max.p=5, max.q=5, max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                       start.p=2, start.q=2, start.P=1, start.Q=1,  stationary=FALSE, seasonal=TRUE)
    fc_reg_gen = forecast(model_gen, h = input$prediction_steps)

    xmm2 = length(myts4)
    consensus_xx = fc_reg_gen$mean 
    scraper2 =  myts4[ c((xmm2 - input$prediction_steps + 1) : xmm2) ] 

    if (sum( scraper2  >  fc_reg_gen$upper[,2] ) > 0)
    {
      consensus_xx[ scraper2 >  fc_reg_gen$upper[,2] ] = scraper2[ scraper2 >  fc_reg_gen$upper[,2] ] 
      } else if( sum(consensus_xx[ scraper2 <  fc_reg$lower[,2]]   ) > 0)
    {
      consensus_xx[ scraper2 <  fc_reg_gen$lower[,2] ] = scraper2 [ scraper2 <  fc_reg_gen$lower[,2] ] 
    }
    
    
    output$Distribution = renderPlot(
      {
        plot(fc_reg_gen)
        points( myts4, col = c('green'))
        points(consensus_xx,col = c('red'))
      }
    )
    
    print("next after:")
    hw_gen <- ets(myts3,model = 'ZZZZ', damped = NULL, allow.multiplicative.trend = TRUE )
    hw_fc_gen <- forecast(hw_gen, h = input$prediction_steps, prediction.interval = T, level = 0.95)
    consensus_fc_gen <- hw_fc_gen$mean
    

    if (sum( scraper2  >  hw_fc_gen$upper ) > 0)
    {
      consensus_fc_gen[ scraper2 >  hw_fc_gen$upper ] = scraper2[ scraper2 >  hw_fc_gen$upper ] 
    } else if( sum(consensus_fc_gen[ scraper2 <  hw_fc_gen$lower]   ) > 0)
    {
      consensus_fc_gen[ scraper2 <  hw_fc_gen$lower ] = scraper2 [ scraper2 <  hw_fc_gen$lower ] 
    }
    
    #print(consensus_fc_gen)
    print('blablabla')
    print(consensus_fc_gen)
    output$Exponential = renderPlot(
      {
        plot(hw_fc_gen)
        points( myts4, col = c('green'))
        points(consensus_fc_gen, col = c('red'))
      }
    )
    print(myts3)

    xreg2 = as.data.frame(globalizedsubset)[,c('distinct_user','total_bets',#'avg_users',
                                               'betamount','bet_player','avg_player','roi','profit')]
    
    print(head(xreg2))
    
    xreg2 =rbind(c(0,0,0), xreg2)
    consensus_prx = consensus_xx
    print(consensus_prx)
    tryCatch({
    
    model_cov_gen = auto.arima(myts3, d=NA, D=NA, max.p=5, max.q=5, max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                           start.p=2, start.q=2, start.P=1, start.Q=1,  stationary=FALSE, seasonal=TRUE, 
                           xreg = as.matrix(xreg2[seq(1,dim(xreg2)[1]-input$prediction_steps - 1) ,]))
    model_pred_cov_gen = forecast(model_cov_gen,h = input$prediction_steps,
                  xreg = as.matrix(xreg2[seq(dim(xreg2)[1]-input$prediction_steps,dim(xreg2)[1] - 1 ),]),
                  bootstrap = TRUE)
    consensus_prx = model_pred_cov_gen$mean 

    #print(consensus_prx)
    #print(model_pred_cov_gen$upper)
    
    if (sum( scraper2  >  model_pred_cov_gen$upper ) > 0)
    {
      consensus_prx[ scraper2 >  model_pred_cov_gen$upper[,2] ] = scraper2[ scraper2 >  model_pred_cov_gen$upper[,2] ] 
    } 
    else if( sum(consensus_fc_gen[ scraper2 <  hw_fc_gen$lower]   ) > 0)
    {
      consensus_prx[ scraper2 <  model_pred_cov_gen$lower[,2] ] = scraper2 [ scraper2 <  model_pred_cov_gen$lower[,2] ] 
    }
    
    print(dim(forecast(model_pred_cov_gen,h = input$prediction_steps,
                       xreg = as.matrix(xreg2[seq(dim(xreg2)[1]-input$prediction_steps,dim(xreg2)[1] - 1 ),]),
                       bootstrap = TRUE)))
    print("het werkt van de eerste keer:")
    print(consensus_prx)
    },
    warning = function(w){
      model_gen = auto.arima(myts3, d=NA, D=NA, max.p=5, max.q=5, max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                             start.p=2, start.q=2, start.P=1, start.Q=1,  stationary=FALSE, seasonal=TRUE)
      fc_reg_gen = forecast(model_gen, h = input$prediction_steps)
      
      xmm2 = length(myts4)
      consensus_prx = fc_reg_gen$mean 
      scraper2 =  myts4[ c((xmm2 - input$prediction_steps + 1) : xmm2) ] 
      
      if (sum( scraper2  >  fc_reg_gen$upper[,2] ) > 0)
      {
        consensus_prx[ scraper2 >  fc_reg_gen$upper[,2] ] = scraper2[ scraper2 >  fc_reg_gen$upper[,2] ] 
      } else if( sum(consensus_prx[ scraper2 <  fc_reg$lower[,2]]   ) > 0)
      {
        consensus_prx[ scraper2 <  fc_reg_gen$lower[,2] ] = scraper2 [ scraper2 <  fc_reg_gen$lower[,2] ] 
      }
      print("oops, er zijn waarschuwingen:")
      print(consensus_prx)

    },
    error = function(e){
      model_gen = auto.arima(myts3, d=NA, D=NA, max.p=5, max.q=5, max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                              start.p=2, start.q=2, start.P=1, start.Q=1,  stationary=FALSE, seasonal=TRUE)
      fc_reg_gen = forecast(model_gen, h = input$prediction_steps)
      

      xmm2 = length(myts4)
      consensus_prx = fc_reg_gen$mean 
      scraper2 =  myts4[ c((xmm2 - input$prediction_steps + 1) : xmm2) ] 
      
      if (sum( scraper2  >  fc_reg_gen$upper[,2] ) > 0)
      {
        consensus_prx[ scraper2 >  fc_reg_gen$upper[,2] ] = scraper2[ scraper2 >  fc_reg_gen$upper[,2] ] 
      } else if( sum(consensus_prx[ scraper2 <  fc_reg$lower[,2]]   ) > 0)
      {
        consensus_prx[ scraper2 <  fc_reg_gen$lower[,2] ] = scraper2 [ scraper2 <  fc_reg_gen$lower[,2] ] 
      }
      print(consensus_prx)
      
    }
    )
    print("time to show your cards:")
    print(fc_reg_gen)
    output$Covariates_Gen = renderPlot(
      {
        plot(fc_reg_gen)
        points( myts4, col = c('green'))
        points(consensus_prx,col = c('red'))
      }
    )
      
     
 
    #print(accuracy(model_cov_gen))
  })  
    
    observeEvent(input$runthrough,{
      
      dates = gamegroup[,c('calculate_date')]
      dates = unique(dates)
      print(dates)
      fc_amt = 0
      hw_amt = 0
      model_amt = 0
      steps = 5
      
      for (k in unique(gamegroup$gametypeid)){
        to_analyze = gamegroup[gamegroup$gametypeid == k,]
        selectionsubset  = merge.data.frame(dates,to_analyze, 
                                            by.x = c('calculate_date'), by.y = c('calculate_date'), all.x=TRUE )
        selectionsubset[is.na(selectionsubset)] = 0.0
        if (( dim(selectionsubset)[1]< 1) || ( dim(dates)[1] < 15 + 5 )) {
          print("not enough data")
          return(0)
        }

        ##specified: arima||exponentially smoothed
        
        myts = ts(selectionsubset[seq(1,dim(selectionsubset)[1]-steps),c(input$parameter)], 
                  start = c(1, 0), frequency = 7 )
        myts2 = ts(as.data.frame(selectionsubset)[seq(1,dim(selectionsubset)[1]),input$parameter], start = c(1,0),
                   frequency = 7 )
        
        xmm = length(myts2)
        scraper =  myts2[ c((xmm - steps + 1) : xmm) ] 
        model = auto.arima(myts, d=NA, D=NA, max.p=5, max.q=5, max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                           start.p=2, start.q=2, start.P=1, start.Q=1,  stationary=FALSE, seasonal=TRUE)
        fc_reg = forecast(model, h = steps)
        fc_amt = sum( (fc_reg$mean - scraper ) * (fc_reg$mean - scraper) ) + fc_amt

                
        hw <- ets(myts,model = 'ZZZZ', damped = NULL, allow.multiplicative.trend = TRUE )
        hw_fc <- forecast(hw, h = steps, prediction.interval = TRUE, level = 0.95)
        hw_amt = sum( (hw_fc$mean - scraper ) * (hw_fc$mean - scraper) ) + hw_amt

        
        print(length(myts[-1]))
        print(myts2)
        xreg2 = as.data.frame(selectionsubset)[,c('distinct_user','total_bets','avg_users','betamount',
                                                  'bet_player','avg_bet','roi','profit')]
        print(xreg2[seq(1,dim(xreg2)[1]-steps - 1) ,])
        #net <- nnet(myts[-1] ~ as.matrix(xreg2[seq(1,dim(xreg2)[1]-steps - 1) ,]) ,
        #            size = 20, rang = 0.1,decay = 5e-4, maxit = 500)
        

        
        model <- keras_model_sequential()
        
        model %>%
          layer_dense(units = 32, input_shape = c(4)) %>%
          layer_activation('relu') %>%
          layer_dense(units = 64) %>%
          layer_dense(units = 1) %>%
          layer_activation('sigmoid')
        
        model %>% compile(
          optimizer = 'rmsprop',
          loss = 'mean_squared_error',
          metrics = c('accuracy')
        )
        
        history <- model %>% fit(
          as.matrix(xreg2[seq(1,dim(xreg2)[1]-steps - 1) ,]),
                    myts[-1], 
          epochs = 300, batch_size = 15, 
          validation_split = 0.2
        )
        
        #yPredInt <- nnetPredInt(net, myts[-1], xreg2[seq(1,dim(xreg2)[1]-steps - 1) ,] ,
        #                        xreg2[seq(dim(xreg2)[1]-steps - 1,dim(xreg2)[1]-1) ,], alpha = 0.05) # 95% confidence interval
        
        #xreg2 = as.data.frame(selectionsubset)[,c('distinct_user','total_bets','betamount','roi')]
        #xreg2 =rbind(c(0,0,0), xreg2)
        #model_cov = auto.arima(myts, d=NA, D=NA, max.p=5, max.q=5, max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
        #                       start.p=2, start.q=2, start.P=1, start.Q=1,  stationary=FALSE, seasonal=TRUE, 
        #                       xreg = as.matrix(xreg2[seq(1,dim(xreg2)[1]-steps - 1) ,]))
        #model_up = forecast(model_cov,h = steps,  xreg = as.matrix(xreg2[seq(dim(xreg2)[1]-steps - 1,dim(xreg2)[1] - 1),]),  bootstrap = TRUE)
        #model_amt = sum( (model_up$mean - scraper) * (model_up$mean-scraper)) + model_amt
        
      }
      print(fc_amt)
      print(hw_amt)
      print(model_amt)
      output$RunThrough2 <- renderDataTable({gamegroup})
    
  })
  
  
})
