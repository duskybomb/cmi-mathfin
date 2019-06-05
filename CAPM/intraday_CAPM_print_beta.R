rm(list=ls())
setwd('~/Projects/ansue_finance_summer_school/data/')
print(load('DATA/BLOCK2/demo_intradaydata.rda'))
library(xts)

stock.name <- "TCS"
stock_id <- 1
day <- 3
interval <- 60
for ( interval in cbind(60, 120, 180, 300, 360, 600)) {
  
  stock <- data.stock.cash[[stock_id]][[day]][, ]
  ## to.period (stock, period = "seconds", k = interval, OHLC = FALSE)
  nifty <- data.nifty[[day]]$nifty/100
  
  ## stock.removed <- stock
  stock.removed <- stock['T09:56:00/T15:30:00']
  nifty.removed <- nifty['T09:56:00/T15:30:00']
  
  interval.data <- function(tick.data) {
    time.data <- lapply (tick.data, function(x) {
      difference.data <- to.period (x, period = "seconds", k = interval, OHLC = FALSE)
      return (difference.data)
    })
    return(time.data)
  }
  
  interval.stock <- interval.data(stock.removed)
  interval.nifty <- interval.data(nifty.removed)
  
  # For Hindlvr day 2
  # interval.nifty <- interval.nifty$nifty[-length(interval.nifty$nifty)]
  
  risk_free_return_annualised <- 6.0
  risk_free_return_daily <- risk_free_return_annualised/250
  
  risk_free_normalised <- risk_free_return_daily/length(interval.stock)
  
  ln_rt_stock <- diff(log(interval.stock$ltp)) - risk_free_normalised
  ln_rt_nifty <- diff(log(interval.nifty$nifty)) - risk_free_normalised
  
  capm_stock <- lm(ln_rt_stock ~ ln_rt_nifty)
  
  capm_stock_analysis<-coefficients(summary(capm_stock))
  capm_stock_analysis <- round(capm_stock_analysis,digits = 5)
  rownames(capm_stock_analysis)<-c("alpha","beta")
  ## Result of capm using lm()
  print(capm_stock_analysis)
  # summary(capm_stock)
  
  # 
  # png(paste(stock.name, '_', day, '_', interval, '.png'))
  # 
  # plot(as.numeric(ln_rt_nifty),as.numeric(ln_rt_stock),xlab="Nifty 50",ylab=stock.name, main=summary(capm_stock)$adj.r.squared)
  # abline(capm_stock,col="blue")
  # grid(col="red")
  # ## Adjusted R-Squareed
  # summary(capm_stock)$adj.r.squared
  # 
  # # data.stock.cash['TCS.cash'][[1]][[1]]
  # 
  # rse<-summary(capm_stock)$sigma
  # al <-capm_stock$coefficients[1]-2*rse
  # b <- capm_stock$coefficients[2]
  # abline(a= al,b= b,col=3,lty=2)
  # au <-capm_stock$coefficients[1]+2*rse
  # abline(a=au,b=b,col=3,lty=2)
  # 
  # dev.off()
}