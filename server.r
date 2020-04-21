rm(list=ls()) 
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(shinydashboard)

library(DT)
library(data.table)
library(dplyr)
library(widyr)
library(gridExtra)
library(tibbletime)
library(tm)
library(timetk)
library(tidyquant)

library(forecast)
library(tseries)
library(quantmod)
library(bsts)

# Plot
library(ggplot2)
library(glue)

server <- function(input, output) {
  
  # Define parameters -----
  gap    <-  windw <- 90 
  slow   <- 30    #(gap: fill # of lags, slow: averaging values for # of days)
  n_avg  <- 26
  fast = macd_n  <- 9 #(averaging values for # of days)
  rnge  <- 150 #(plot lines with # days)
  dai   <- 30  #(# horizons to predict)
  niter <- 50 #(iteration of bsts)
  w1dth <- 1.5 #(threshold to check k d lines)
  cutoff <- 51*5*2
  thres <- as.Date("2015-02-01") # analyse target series after this date
  
  # function for transform xts to data.table ---
  trans.dat <- function(df){
    colnames(df)[1] <- c("index")
    df$index  <- as.Date(df$index)
    colnames(df) <- c("Date","Open","High","Low", "Close", "Volume", "Adjusted")
    df$lag1 <- c(-1, df$Close[1:(length(df$Close)-1)])
    df$lag7 <- c(rep(-1,7), df$Close[1:(length(df$Close)-7)])
    df}
  
  # function for calculating KD lines
  kd   <- function(target, gap, slow, fast, macd_n, n_avg){
    target_hi  <- target$High
    target_lo  <- target$Low
    target_C   <- target$Close
    res1       <- data.table(matrix(data= -2, ncol = 8, nrow = length(target_C)))
    colnames(res1) <- c('Hi', 'Lo','k','d','l', 'k_d', 'rsv', 'osc')
    res1$date      <- target$Date
    #res1$Volume    <- target$Volume
    
    # caluculate exponential factor
    fast_factor <- rev((1/sum(1/seq(2, 2*fast, 2)))/seq(2, 2*fast, 2)) # giving more weight in the recent values
    slow_factor <- rev((1/sum(1/seq(2, 2*slow, 2)))/seq(2, 2*slow, 2))
    macd_factor <- rev((1/sum(1/seq(2, 2*macd_n, 2)))/seq(2, 2*macd_n, 2))
    
    res_hi= res_lo = res1$rsv = d1f = res1$macd = res1$osc = res1$a= res1$b =res1$d=res1$l = res1$k_d = res1$k = res1$d   <- 0
    for (i in 1:length(target_C)){
      if(i >  gap) {
        res_hi[i] <- max(target_hi[(i-13): i]) # replace gap to 14, only traack back to 14 days
        res_lo[i] <- min(target_lo[(i-13): i])
        res1$rsv[i]    <- 100*((target_C[i]-res_lo[i]) /(res_hi[i]-res_lo[i]))
        res1$k[i]      <- 2/3*(res1$k[i-1]) + 1/3*res1$rsv[i]
        res1$d[i]      <- 2/3*(res1$d[i-1]) + 1/3*res1$k[i]
        #res1$osc[i]    <- (sum(fast_factor*(target_C[(i-fast+1) :i])) - sum(slow_factor*(target_C[(i-slow+1) :i]))) 
        #             -(sum(macd_factor*d1f[(i-macd_n+1):i]))
        res1$osc[i]    <- ((target_C[i]-res_lo[i])/(res_hi[i]-res_lo[i]))
        res1$l[i]      <- mean(target_C[(i-n_avg+1) :i])
        res1$k_d[i]    <- abs(res1$d[i]-res1$k[i])
        res1$target_C[i] <- target_C[i]
      }
      else {
        res_hi[i] <- res_lo[i] <-res1$l[i] <- res1$rsv[i]<- -1
        #res_hi[i] <- res_lo[i]<- a[i] <- b[i] <-l[i] <- rsv[i]<- -1
        res1$k[i] <- res1$d[i] <- res1$k_d[i]  <-  50
        res1$osc[i] <- -100
        #osc[i] <- d1f[i] <-macd[i] <- -100
        res1$target_C[i] <- target_C[i] }
      
      res1$osc[i]   <- ifelse(res1$osc[i] != -100,res1$osc[i],0)
    }
    res1[,3:length(names(res1))]
  }
  
  # function to show plots ---
  plot.show1 <- function(x, pname, windw, w1dth){
    data <- x[(length(x$target_C)-(windw)) : length(x$target_C)]
    ggplot(data = data, aes(x=date)) +
      geom_line(aes(y=k, colour = "K"), size= 1.0) +
      geom_line(aes(y=d, colour = "D"), size= 1.0) +
      geom_bar(aes(x = date, y = 10*osc, colour = 'osc'), stat = 'identity', width = 0.7, fill= 'lightgray', colour= 'lightgray') +
      geom_hline(yintercept = c(20, 80))+
      #geom_bar(aes(x = date, y = ifelse(abs(k_d) < w1dth, max(k), 0), colour = "k_d"), stat = 'identity', width = 0.05, color = "steelblue", linetype = 'dotted') +
      theme_bw() + theme(legend.title = element_blank()) + 
      ylab("") + xlab("") +
      ggtitle(paste0(max(as.Date(x$date)),' : ', toupper(pname),' ', windw, '-day Analysis ', ' Window: ', w1dth)) + 
      theme(axis.text.x=element_text(angle = -60, hjust = 0),
            plot.background=element_rect(fill = 'white'),
            panel.background = element_rect(fill = "white"),
            panel.border = element_blank(),
            panel.grid.minor = element_line(colour="gray", size=0.3),
            panel.grid.major = element_blank(),
            legend.background = element_rect(fill = "white", color = NA),
            legend.key = element_rect(color = "white", fill = "white"),
            # Change axis line
            axis.line = element_line(colour = "black"))
    
  }
  
  # ARMA Forecasting
  # Test stationarity of the time series Augmented Dickey-Fuller Test (ADF) 
  arma1a <- function(df){
    train  <- df$Close[(length(df$Close)-cutoff):(length(df$Close)-dai)]
    # test of stationarity
    pval    <- adf.test(ts(train), alternative="stationary")
    station <- ifelse(pval$p.value < 0.05, TRUE, FALSE )     
    auto.arima(ts(train), stationary = station, seasonal = TRUE)
  }
  
  # ARMA Result, store real and predicted values in a table
  arma1_dt <- function(df){
    arimax  <- arma1a(df)
    data.frame(c(as.numeric(fitted(arimax)), as.numeric(predict(arimax, n.ahead =dai)$pred)),
                     df$Close[(length(df$Close)-cutoff):(length(df$Close))],
                       as.Date(df$Date[(length(df$Close)-cutoff):(length(df$Close))])
    )
    }
  
  pred_arma1a   <- function(df){
    d1   <- arma1_dt(df)
    names(d1) <- c("Fitted", "Actual", "Date")
    train  <- df$Close[(length(df$Close)-cutoff):(length(df$Close)-dai)]
    MAPE <- d1[length(train):(length(train)+dai),] %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))
    print(paste0("Arima MAPE:",MAPE))
  }
  
  # Plotting ARMA Result
  arma_plot <- function(df, pname){
    arimax <- arma1a(df)  # call model
    p      <- forecast(arimax, level = c(95), h = dai) # prediction
    d1     <-  arma1_dt(df) # call store predicted values  
    names(d1) <- c("Fitted", "Actual", "Date")
    train  <- df$Close[(length(df$Close)-cutoff):(length(df$Close)-dai)]
    MAPE <- d1[length(train):(length(train)+dai),] %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))
    
    # turn interval to dataframe
    posterior.interval <- cbind.data.frame(
      as.numeric(p$lower),
      as.numeric(p$upper),
      as.Date(d1$Date)[(length(d1$Date)-dai+1):length(d1$Date)]   )
      #as.Date(as.Date(d1$Date)-dai) + seq(1,dai))
    names(posterior.interval) <- c("LL", "UL", "Date")
    
    data <- left_join(d1, posterior.interval, by="Date")
    
    mark <- as.numeric(as.Date(df$Date[(dim(df)[1]-dai)]))#:dim(df)[1]]))
    ### Plot actual versus predicted
    g <-ggplot(data=data, aes(x=Date)) +
      geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
      geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=2) +
      theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
      geom_vline(xintercept=mark, linetype=2) + 
      geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
      ggtitle(paste0(pname," : ARIMA -- Holdout MAPE = ", round(100*MAPE,2), "%")) +
      theme(axis.text.x=element_text(angle = -90, hjust = 0))
     return(g) 
   }
  
  
  pred_arma1b <- function(df){      
    dai = 10
    # prediction table
    x <- data.table(matrix(data= rep(-1,dai), ncol = 1, nrow = dai))
    m1  <- arma1a(df)
    arma_pred   <- forecast(m1, level = c(95), h = dai)
    x$Date <- as.Date(max(df$Date)) + seq(1,dai)
    x$Mean <- round(arma_pred$mean[1:dai],2)
    x$Low  <- round(arma_pred$lower[1:dai],2)
    x$High <- round(arma_pred$upper[1:dai],2)
    x[,-1]
  }
  
  # Baysein 
  bsts_model <- function(df){
    #target <- df$Close
    
    #' Model train with one year data and test on 3weeks
    train  <- df$Close[(length(df$Close)-cutoff):(length(df$Close)-dai)]
    ss     <- AddLocalLinearTrend(list(),train)
    ss     <- AddSeasonal(ss, train, nseasons = 52, season.duration = 6)
    
    # Forecast with BSTS
    bsts(train, state.specification = ss, niter = niter, ping=0, seed=2019)
    
    ### Get a suggested number of burn-ins
    #burn <- SuggestBurn(0.2, bsts.model)
    #p    <- predict.bsts(bsts.model, horizon = dai, burn = burn, quantiles = c(.025, .975))
  }
  
  # Manipulate data
  bsts_plot <- function(df, pname){

    train  <- df$Close[(length(df$Close)-cutoff):(length(df$Close)-dai)]
    #df     <- train
    bsts.model <- bsts_model(df)
    burn <- SuggestBurn(0.2, bsts.model) 
    p    <- predict.bsts(bsts.model, horizon = dai, burn = burn, quantiles = c(.025, .975))
    ## data prep
    d2 <- data.frame(
      # fitted values and predictions
      c(as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),])+train),  
        as.numeric(p$mean)),
      # actual data and dates 
      as.numeric(df$Close[(length(df$Close)-cutoff):(length(df$Close))]),
      as.Date(df$Date[(length(df$Close)-cutoff):(length(df$Close))]))
    names(d2) <- c("Fitted", "Actual", "Date")
    
    ### MAPE (mean absolute percentage error)
    MAPE <- d2[length(train):(length(train)+dai),] %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))
    
    ### 95% forecast credible interval
    posterior.interval <- cbind.data.frame(
      as.numeric(p$interval[1,]),
      as.numeric(p$interval[2,]), 
      d2$Date[(length(train)+1):(length(train)+dai)] )
    names(posterior.interval) <- c("LL", "UL", "Date")
    
    data <- left_join(d2, posterior.interval, by="Date")
    #data <- bsts1(df)
    mark <- as.numeric(as.Date(df$Date[(dim(df)[1]-dai)]))#:dim(df)[1]]))
    g <-ggplot(data=data, aes(x=Date)) +
      geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
      geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=2) +
      theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
      geom_vline(xintercept=mark, linetype=2) + 
      geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
      ggtitle(paste0(pname," : BSTS -- Holdout MAPE = ", round(100*MAPE,2), "%")) +
      theme(axis.text.x=element_text(angle = -90, hjust = 0))
    return(g)
  }
  
  # Print out MAPE
  pred_bsts1a <- function(df){
    
    # Model train with one year data and test on 3weeks
    
    train  <- df[(length(df$Close)-cutoff):(length(df$Close)-dai), c('Date', 'Close')]
    test   <- df[(length(df$Close)-(dai-1)):length(df$Close), c('Date', 'Close')]
    ml     <- bsts_model(df)
    burn <- SuggestBurn(0.2, bsts_model(df)) 
    p    <- predict.bsts(bsts_model(df), horizon = dai, burn = burn, quantiles = c(.025, .975))
    
    test_pred <- as.numeric(p$mean)
    errors    <- test$Close-test_pred
    mape   <- mean(abs(errors)/test$Close)*100-1
    print(paste0("BSTS MAPE:",mape))}
  
  # Predict values of next 30 horizontals
  pred_bsts1b <- function(df){
    dai = 10
    train  <- df[, c('Date', 'Close')] #(length(df$Close)-cutoff):(length(df$Close))
    ml     <- bsts_model(train)
    burn   <- SuggestBurn(0.2, bsts_model(train)) 
    p      <- predict.bsts(bsts_model(train), horizon = dai, burn = burn, quantiles = c(.025, .975))
    
    fut_pred <- data.table(matrix(data= -2, ncol = 4, nrow = dai))
    names(fut_pred) <- c("Date","Mean","Low", "High")
    fut_pred$Date   <- as.Date(max(df$Date)) + seq(1,dai)
    fut_pred$Mean   <- round(p$mean,2)
    fut_pred$Low    <- round(p$interval[1,],2)
    fut_pred$High   <- round(p$interval[2,],2)
    
    return(fut_pred)
    }
  
  # Calculate MAPE for ARIMA and BSTS 
  pred_mape   <- function(df){
    d1        <- arma1_dt(df)
    names(d1) <- c("Fitted", "Actual", "Date")
    train1    <- df$Close[(length(df$Close)-cutoff):(length(df$Close)-dai)]
    train     <- df[(length(df$Close)-cutoff):(length(df$Close)-dai), c('Date', 'Close')]
    test      <- df[(length(df$Close)-(dai-1)):length(df$Close), c('Date', 'Close')]
    
    # bsts error
    ml <- bsts_model(df)
    burn <- SuggestBurn(0.2, bsts_model(df)) 
    p    <- predict.bsts(bsts_model(df), horizon = dai, burn = burn, quantiles = c(.025, .975))
    test_pred <- as.numeric(p$mean)
    errors    <- test$Close-test_pred
    MAPE_b   <- mean(abs(errors)/test$Close)
    # arima error
    MAPE_m <- d1[length(train1):(length(train1)+dai),] %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))
    
    print(paste0("Arima MAPE:",MAPE_m))
    print(paste0("BSTS MAPE:",MAPE_b))
  }
  
  #------------------------- end of function ---------------
  #   output$symbol <- renderText({ isolate({input$stockId})})
  id1a        <- reactive({input$stockId})
  id1         <- eventReactive(input$action, isolate(id1a())) 
  #id <- reactive({isolate({input$stockId})})
  id <- reactive({ifelse(input$stockId != "",id1(), 'fisv')})
  
  ori <-reactive({
    print(id())
    target <- getSymbols(id(), src='yahoo',auto.assign=TRUE) #input$stockId
    get(target)
  })
  
  plot_trend <- reactive({
    target <- getSymbols(id(), src='yahoo',auto.assign=TRUE)
    chart_Series(ori(),name=target, theme =chart_theme(), TA="add_Vo();add_BBands()")
  })
  
  df1 <- reactive({
    as.data.table(na.omit(ori()))
    #na.omit(x)
  })
  
  getContents   <- reactive({trans.dat(df1()[df1()[,as.Date(index)> thres]])}) # main stock table
  kd_data       <- reactive({kd(getContents()[,c("Date", "High", "Low", "Close")],gap,slow, fast, macd_n, n_avg)}) # table with KD calculated #, "Volume"
  arima_l       <- reactive({target <- getSymbols(id(), src='yahoo',auto.assign=TRUE)
                   arma_plot(getContents(),target)})
  bsts_l        <- reactive({target <- getSymbols(id(), src='yahoo',auto.assign=TRUE)
                   bsts_plot(getContents(),target)})
  kd_line       <- reactive({plot.show1(kd_data(), id() , rnge, w1dth)}) #input$stockId
  MAPE_l        <- reactive({pred_mape(getContents())})
                            
  
  # react after a click
  #eventReactive(input$stockId, {ori()})
  #id1         <- eventReactive(input$enter, id1a()) 
   #plot_dat <- 
  
  plot_arima <- eventReactive(input$show_arima,{arima_l()})
  plot_bsts  <- eventReactive(input$show_bsts, {bsts_l()})
  plot_kd    <- eventReactive(input$show_kd, {kd_line()})
  MAPE_show  <- eventReactive(input$show_error,{MAPE_l()})
  plot_dat   <- eventReactive(input$show, {plot_trend()})
  fully_filtered <- eventReactive(input$select, {getContents()[order(-Date)]}) #[order(-Date)]
  
  # output data here ----
  
  # stock trend
  output$trend_plot <- renderPlot({
    #data <- getContents()
    if (is.null(getContents())==TRUE){print('Wait ...')}
    else{plot_trend()} #plot_dat()
  }) 
  
  # print systime
  output$systime <- renderPrint({
    if (is.null(getContents())==TRUE) {print('Wait ...')}
    else{
      strt <- Sys.time()
      kd_line()
      mid.t <- Sys.time()
      plot_kd()
      end.t <- Sys.time()
      x <- mid.t - strt
      x1 <- end.t - mid.t
      print(paste0('data prep: ', x,
                   ' and graph prep: ', x1))
    }
  })
  
  # KD lines
  output$kd_plot <- renderPlot({
    if (is.null(getContents())==TRUE){print('Wait ...')}
    else{plot_kd()}
  }) 
  # arima lines
  output$arima_plot <- renderPlot({
    if (is.null(getContents())==TRUE){print('Wait ...')}
    else{plot_arima()}
  })
  
  # bsts lines
  output$bsts_plot <- renderPlot({
    if (is.null(getContents())==TRUE){print('Wait ...')}
    else{plot_bsts()}
  })
  
  # MAPE text
  output$mape_text <- renderPrint({
    if (is.null(getContents())==TRUE){print('Wait ...')}
    else{MAPE_show()} }) #pred_arma1a(getContents())
  
  # arima table
  output$arma_table <- DT::renderDataTable(
    DT::datatable({
      #data <- getContents()
      if (is.null(getContents())==TRUE){print('Wait ...')}
      else{pred_arma1b(getContents())}
    }))
  
  # bsts table
  output$bsts_table <- DT::renderDataTable(
    DT::datatable({
      #data <- getContents()
      if (is.null(getContents())==TRUE){print('Wait ...')}
      else{pred_bsts1b(getContents())}
    }))
  
  # stock table
  output$x1 <- DT::renderDataTable(DT::datatable({
    (fully_filtered()[1:10,c("Date","Close")]) }))
  
  # Download filtered data ----
  output$x2 = downloadHandler('filtered.csv', content = function(file) {
    s = input$x1_rows_all
    write.csv(fully_filtered()[s, , drop = FALSE], file)
  })
}
