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
options(stringsAsFactors=FALSE)


handle = odbcDriverConnect('driver=SQL Server;server=DESKTOP-UVDOJ3H\\MSSQLSERVER16;')
selections = sqlQuery(handle,paste("select sum(total_bets) nbets, sum(total_bet) betamount , sum(total_return) tot_return , gametypeid  , [name], gamesuppliertype,
                                   convert(float, sum(total_bet)) / convert(float,sum(total_bets)) avg_bet, 
                                   convert(float, sum(total_return)) / convert(float, case when sum(total_bet) =0 then 1 else sum(total_bet) end ) roi, wsite
                                   from casinocash.dbo.memberid_gametypeid gt join 
                                   casinocash.dbo.gametype tg on tg.id = gt.gametypeid 
                                   group by gametypeid, [name],gamesuppliertype, wsite
                                   having sum(total_bets) > 10000
                                   order by tot_return desc 
                                ", sep=''), stringsAsFactors = FALSE)
bets_per_user = sqlQuery(handle,paste("select memberid,gametypeid,convert(float,sum(total_bet)) / convert(float,sum(total_bets)) avg_bet, 
                                      convert(float,sum(total_return)) / convert(float, case when sum(total_bet) =0 then 1 else sum(total_bet) end ) roi,
                                      sum(total_bets) nbets, sum(total_bet) BetAmount, sum(total_return) payoff, wsite
                                      from casinocash.dbo.memberid_gametypeid group by memberid, gametypeid, wsite", sep=''))


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  values <- reactiveValues()
output$websites <- renderUI({
  selectInput(inputId = "websites", selected = "DESKTOP-UVDOJ3H\\MSSQLSERVER16_CasinoCash",
                                    label = "websites", multiple = FALSE, 
                                    choices = unique(sort(selections$wsite))) })
output$games <- renderUI({
  selectInput(inputId = "games", selected = "615",
            label = "games", multiple = TRUE, 
            choices = unique(sort(selections$gametypeid))) })

#observeEvent(input$games,{
#  selections2 = data.table(selections)
#  output$table_games <- renderDataTable({ selections[selections2$gametypeid %in% input$games, ] })
#  filtered = subset(selections2, gametypeid %in% input$games)
#  filtered = filtered[, list(nbets = sum(nbets),betamount = sum(betamount), 
#                                                             total_return = sum(tot_return)), by = list(gametypeid) ]
#  filtered$roi = filtered$total_return / filtered$betamount
#  selections3 = selections2[, list(nbets = sum(nbets),betamount = sum(betamount), 
#                             total_return = sum(tot_return)), by = list(wsite) ]
#    selections3$roi = selections3$total_return / selections3$betamount
#    
#    observeEvent(input$Cumuldist,{
#        if((input$Cumuldist == 'nbets') || (input$Cumuldist == 'betamount'))
#        {
#        setorderv(selections3,cols = input$Cumuldist, order =-1)
#        selections3[,'prob'] = selections3[[input$Cumuldist]] / sum(selections3[[input$Cumuldist]])
#        selections3[,'cumprob'] =  cumsum(selections3[[input$Cumuldist]]) / sum(selections3[[input$Cumuldist]])
#        setorderv(filtered,cols = input$Cumuldist, order =-1)
#        filtered[,'prob'] = filtered[[input$Cumuldist]] / sum(filtered[[input$Cumuldist]])
#        filtered[,'cumprob'] = cumsum(filtered[[input$Cumuldist]]) / sum(filtered[[input$Cumuldist]])
#        }  else if (input$Cumuldist == 'total_return')
#      {
#          observeEvent(input$roicoff,{
#          filtered = filtered[ filtered$roi < input$roicoff,]
#          filtered$mroi = filtered$roi + input$roicoff
#          filtered$mpayoff = filtered$mroi * filtered$betamount
#          setorderv(filtered,cols=input$Cumuldist, order = -1)
#          filtered[,'prob']= filtered[['mpayoff']] / sum(filtered[['mpayoff']])
#          filtered[,'cumprob']= cumsum(filtered[['mpayoff']]) / sum(filtered[['mpayoff']])
#          
#          selections3 = selections3[ selections3$roi >= input$roicoff,]
#          selections3$mroi = selections3$roi + input$roicoff
#          selections3$mpayoff = selections3$mroi * selections3$betamount
#          setorderv(selections3,cols=input$Cumuldist, order = -1)
#          selections3[,'prob']= selections3[['mpayoff']] / sum(selections3[['mpayoff']])
#          selections3[,'cumprob']= cumsum(selections3[['mpayoff']]) / sum(selections3[['mpayoff']])
#          })
#        }
#      output$aggregated_games <-renderDataTable({filtered})
#      output$per_website <- renderDataTable({selections3})
#    })
#    
#})



observeEvent(input$websites,{
     if (input$websites != ''){
     updateSelectInput(session, inputId = "gametypeids", selected = c(602),
                       choices =sort(unique(selections[selections$wsite == input$websites,'gametypeid']) ))
     }
     })

  output$myTable <- renderDataTable({ dataOverview() })
  #output$CasinoCash <-renderDataTable({querydata()})
  output$UserAggregates <- renderDataTable({queryagg()})
  output$userDensity <- renderPlot({if (!is.null(input$gametypeids)) {plot(density(filter_on_bet_amount()$roi))} })
  output$basicDensity <- renderPlot({userset = querydata(); if(!is.null(userset$roi)){ plot(density(filterg_on_bet_amount()$roi))}  })
  output$selected_userstats = renderTable({data = queryagg();print(data); return(data[data$memberid == input$UserAtRisk, ])})
  output$basiccDensity <- renderPlot({userset = querydata(); if(!is.null(userset$roi)){ plot(density(filterg_on_bet_amount()$roi))}  }) 
  
    
  connect = reactive({
    if (values$target != '')
    {
      print(values$target)
      parsed = strsplit(values$target,'_')
      handle = c()
      if (parsed[[1]][1] == "DESKTOP-UVDOJ3H\\MSSQLSERVER16")
      {
        handle = odbcDriverConnect('driver=SQL Server;server=DESKTOP-UVDOJ3H\\MSSQLSERVER16;')
      } else if( parsed[[1]][1] == "10.80.17.190")
      {
        handle = odbcConnect("DB190", uid="BI.Tom", pwd='5jkl$BGI') 
      } else if(parsed[[1]][1] == "10.80.17.51")
      {
        handle = odbcConnect("DB051", uid="BI.Tom", pwd='Nostradamus846')
      }
      return(list(handle = handle,db = parsed[[1]][2]))
    }
  
  })
  
  connection = reactive({
    if (!is.null(input$websites))
    {
    parsed = strsplit(input$websites,'_')
    handle = c()
    if (parsed[[1]][1] == "DESKTOP-UVDOJ3H\\MSSQLSERVER16")
    {
      handle = odbcDriverConnect('driver=SQL Server;server=DESKTOP-UVDOJ3H\\MSSQLSERVER16;')
    } else if( parsed[[1]][1] == "10.80.17.190")
    {
      handle = odbcConnect("DB190", uid="BI.Tom", pwd='5jkl$BGI') 
    } else if(parsed[[1]][1] == "10.80.17.51")
    {
      handle = odbcConnect("DB051", uid="BI.Tom", pwd='Nostradamus846')
    }
    return(list(handle = handle,db = parsed[[1]][2]))
    }
  })
    
 # observe({
 #   if (input$selectall > 0) {
#      if (input$selectall %% 2 == 0){
 #       print("active")
#        print(unique(selections[,'gametypeid']))
#        updateSelectInput(session,inputId ="games" , selected = unique(sort(selections$gametypeid)))
 #     } else {
 #       print("not-active")
#        updateSelectInput(session,inputId ="games" , selected = c())
#      }
 #   }
#  })
  
  datalistener = observeEvent(querydata(),{
    rawdata = querydata()
    odds = rawdata[, list(count = length(BetAmount)) , by = c('roi')]
    odds$probs = odds$count / sum(odds$count)
    output$oddstable = renderTable(odds[order(odds$roi),])
    values$odds = odds
    
    if (input$m_cutoff == "Yes")
    {
      asymptotic = replicate(100000,mean(sample(odds$roi, 10000 , replace = TRUE,prob = odds$count)))
      asymptotic = fitdist(asymptotic,"norm")
      
      for (k in c(15,25, 50,100,200,500,1000,2000, 5000,10000))
      {
        testsample = replicate(100000,mean(sample(odds$roi, k , replace = TRUE,prob = odds$count)))
        testvalue = cvm.test(testsample, "pnorm", asymptotic$estimate[1], asymptotic$estimate[2]* sqrt(10000) / sqrt(k))$p.value
        if (testvalue < 0.01)
        {
          print("cutoff to save is:")
          print(k)
          values$cutoff = k
          values$asymptotic = asymptotic
          print(asymptotic)
          
          break
        }
      }
    }
    updateSelectInput(session,inputId ="comparesite" , selected = input$websites ,choices = unique(sort(
            selections[selections$gametypeid == input$gametypeids,'wsite'])))
    
  })
  
  observeEvent(input$comparesite,
    {
      print(input$comparesite)
      print("querying")
      values$target =input$comparesite
      conn = connect()
      print(conn)
      if ((!is.null(conn)) && (conn$handle != -1))
      {
        
      distributions = sqlQuery(conn$handle,paste("select top 500000 *, Payoff / BetAmount roi from 
                                                 [", conn$db,"].dbo.betrecord where gametypeid=", input$gametypeids, 
                                                 sep=''))
      distributions = data.table(distributions)
      output$cSite = renderPlot({plot(density(distributions$Payoff / distributions$BetAmount))})
      odds = distributions[, list(count = length(BetAmount)) , by = c('roi')]
      odds$probs = odds$count / sum(odds$count)
      odds =odds[order(odds$roi),]
      values$odds = values$odds[order(values$odds$roi),]
      print("cutoff in this case is:")
      print(values$cutoff)
      output$compoddstable = renderPlot({
      plot(density(replicate(100000,mean(sample(odds$roi, values$cutoff , replace = TRUE,prob = odds$count)))),col = 'red') +
      lines(density(replicate(100000,mean(sample(values$odds$roi, values$cutoff , replace = TRUE,prob = values$odds$count)))),
              col = 'blue') +
      lines(density(rnorm(10000, values$asymptotic$estimate[1], values$asymptotic$estimate[2]*sqrt(10000) / sqrt(values$cutoff))), col='green', type='l')
        
      })
      } 
      # aggregate = sqlQuery(conn$handle,paste("select memberid,gametypeid, sum(wagerscount) nbets,sum(betamountsum) 
      #BetAmount, sum(payoffsum) payoff, sum(commissionablesum) commissionable,
      #sum(payoffsum) / sum(betamountsum ) roi from [",conn$db,"].dbo.betrecordquerygamesum where gametypeid = ", input$gametypeids,
      #" group by memberid, gametypeid ", sep=''))
    })
    
    

  
  
  
    ##empirical sampling
    observeEvent(input$random_samples,{
    odds = values$odds

    if(! is.null(odds)){
    data = queryagg(); selected = data[data$memberid == input$UserAtRisk, ]
    n  = input$random_samples
    if (n == -1){ n = selected$nbets; }  
    classes = replicate(100000,mean(sample(odds$roi, n , replace = TRUE,prob = odds$count)))
    
    output$sampledDataPlot = renderPlot(plot(density(classes)))
    print(input$NormDist)
    res  = quantile(classes, c(1:1000)/ 1000)
    if (input$NormDist == 'Yes'){
      example = fitdist(classes,"norm")
      output$VisNorm = renderPlot(plot(example))
      print(gofstat(example))
      if (input$random_samples == -1 ) {selected$test_normal = pnorm(selected$roi, example$estimate[1], example$estimate[2]) } 
      output$sampledDataPlot = renderPlot({
        plot(density(classes))  +
        lines(density(rnorm(10000, example$estimate[1], example$estimate[2])), col='red', type='l')
      })
    }
    if (input$NormDist =='Limit')
    {
      example = fitdist(classes,"norm")
      output$VisNorm = renderPlot(plot(example))
      asymptotic = replicate(100000,mean(sample(odds$roi, 10000 , replace = TRUE,prob = odds$count)))
      asymptotic = fitdist(asymptotic,"norm")
      print(gofstat(example))
      print(example)
      print(gofstat(asymptotic))
      print(asymptotic)
      
      if (input$random_samples == -1 ) {
        selected$test_normal = pnorm(selected$roi, example$estimate[1], example$estimate[2]) 
        selected$asym_normal = pnorm(selected$roi, asymptotic$estimate[1], asymptotic$estimate[2] * sqrt(10000) / sqrt(n))
      }
      print(n)
      output$sampledDataPlot = renderPlot({
        plot(density(classes))  +
          lines(density(rnorm(10000, example$estimate[1], example$estimate[2])), col='red', type='l') +
          lines(density(rnorm(10000, asymptotic$estimate[1], asymptotic$estimate[2] * sqrt(10000) / sqrt(as.integer(n)))), col='orange', type='l')
      })
    }
    
    if ( input$random_samples == -1 ){
    selected$test = max(which(selected$roi > res)) / 1000
    output$selected_userstat = renderTable(selected)   
    }
    }
    
    })
  
  filter_on_bet_amount <- reactive({
    users = queryuser()
    print(input$BetAmount)
    temp =   users[log(users$betamount) > as.double(input$BetAmount), ] 
    return(temp)
  })

  filterg_on_bet_amount <- reactive({
    users = querydata()
    print(input$BetAmountG)
    temp =   users[log(users$BetAmount) > as.double(input$BetAmountG), ] 
    return(temp)
  })
  
  observeEvent( querydata(),ignoreNULL = TRUE,{
    users = querydata()
    if(!is.null(users)){
    updateSliderInput(session,"BetAmount",min = min(log(users$BetAmount)), max = max( log(users$BetAmount)), value = 0)
    updateSliderInput(session, "BetAmountG",min = min(log(users$BetAmount)), max = max( log(users$BetAmount)), value = 0)
    }
    })
  
  observeEvent(queryagg(), ignoreNULL =TRUE,{
    users = queryagg()
    if(!is.null(users)){
    updateSelectInput(session, "UserAtRisk", choices = unique(users$memberid))
    output$nbets_dist = renderPlot(plot(density(users$nbets )))
    output$betamount_dist = renderPlot(plot(density(users$BetAmount)))
    fp_nbets <- fitdist(users$nbets, "gpd",method='mge',gof='CvM',start=c(xi=0.7,mu=1.2, beta = 1))
    output$nbets_fit_dist = renderPlot(plot(fp_nbets))
    fp_nbets_tail <- fitdist(bets_per_user$nbets, "gpd",method='mge',gof='CvM',start=c(xi=0.7,mu=1.2, beta = 1))
    output$nbets_tail_dist = renderPlot(plot(fp_nbets_tail))
    print(fp_nbets)
    print(fp_nbets_tail)
    print(gofstat(fp_nbets))
    print(gofstat(fp_nbets_tail))
    fp_betamount <- fitdist(users$BetAmount, "gpd",method='mge',gof='CvM',start=c(xi=0.7,mu=1.2, beta = 1))
    output$betamount_fit_dist = renderPlot(plot(fp_betamount))
    fp_betamount_tail <- fitdist(bets_per_user$BetAmount, "gpd",method='mge',gof='CvM',start=c(xi=0.7,mu=1.2, beta = 1))
    output$betamount_tail_dist = renderPlot(plot(fp_betamount_tail))
    print(fp_betamount)
    print(fp_betamount_tail)
    
    }
    })
  
  
     dataOverview = reactive({
    return(selections[selections$wsite== input$websites,]) 
  })
  
   
   
   
  queryuser = reactive({
    conn=connection()
    if ((!is.null(conn)) && (conn$handle != -1))
    {
    print(input$gametypeids)
    print(input$UserAtRisk)
    users = sqlQuery(conn$handle,paste("select memberid, gametypeid, betamount, payoff , commissionable, payoff / betamount roi
                                  from [",conn$db,"].dbo.betrecord where gametypeid = ", input$gametypeids," and memberid = ",
                                  input$UserAtRisk, sep =''))
    if ((is.data.frame(users)) && (dim(users)[1] > 0))
    {
    users = data.table(users)
    return(users)
    }
    }
  })
  
  queryagg = reactive({
  #  conn = connection()
  #  if ((!is.null(conn)) && (conn$handle != -1))
  #  {
    print(input$gametypeids)  
     aggregate = subset(bets_per_user, gametypeid == input$gametypeids  & wsite == input$websites)
 #   aggregate = sqlQuery(conn$handle,paste("		select memberid,gametypeid, sum(wagerscount) nbets,sum(betamountsum) 
 #BetAmount, sum(payoffsum) payoff, sum(commissionablesum) commissionable, sum(payoffsum) / sum(betamountsum ) roi 
 #                                      from [",conn$db,"].dbo.betrecordquerygamesum where gametypeid = ", input$gametypeids,
 #                                      " group by memberid, gametypeid ", sep='')) 
    if ((is.data.frame(aggregate)) && (dim(aggregate)[1] > 0)){
      aggregate = data.table(aggregate)
      return(aggregate)
    }
#    }
  })
    
  querydata = reactive({
    conn = connection()
    if ((!is.null(conn)) && (conn$handle != -1))
    {
    print(input$gametypeids)  
    userset = sqlQuery(conn$handle,paste("select top 500000 * from [", conn$db,"].dbo.betrecord where gametypeid=", input$gametypeids, sep=''))
    if ((is.data.frame(userset)) && (dim(userset)[1] > 0)){
    userset$roi = userset$Payoff / userset$BetAmount 
    userset = data.table(userset)
    return(userset)
    }
    }
  })
  
    getChoices = reactive({ 
    return( as.list(unique(  selections[ , 'gametypeid' ] ))) 
    })
 
  

})