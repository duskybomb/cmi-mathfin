rm(list=ls())
setwd('~/Projects/ansue_finance_summer_school/data/')
print(load('DATA/BLOCK2/demo_intradaydata.rda'))
library(xts)

stock.name <- "TCS"
stock_id <- 2
day <- 4
interval <- 300

daily_gains = seq(1, 4, 1)
stock_gains = seq(1, 15, 1)
# df.stocks <- data.frame(cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
# colnames(df.stocks) <- cbind('stock_id', 'day', 'interval', 'alpha', 'beta', 'alpha_std', 'beta_std', 'alpha_t', 'beta_t', 'alpha_p', 'beta_p', 'r_adj')
for (stock_id in seq(1, 15, 1)) {
  # print(stock_id)
  for (day in seq(1, 4, 1)) {
#     for ( interval in cbind(60, 120, 180, 300, 360, 600)) {
        
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
  if ((stock_id == 2 & (day == 2 | day == 4)) |
      (stock_id == 4 & day == 2) |
      (stock_id == 5 & day == 1) |
      (stock_id == 7 & day == 4) |
      (stock_id == 8) | 
      (stock_id == 12 & (day == 2 | day == 4)) |
      (stock_id == 13 & day == 2)) {
    interval.nifty <- interval.nifty$nifty[-length(interval.nifty$nifty)]
  } else if (stock_id == 10 | stock_id == 11 | stock_id == 14 | stock_id == 15) {
    break
  }
  
  risk_free_return_annualised <- 6.0
  risk_free_return_daily <- risk_free_return_annualised/250
  
  risk_free_normalised <- risk_free_return_daily/length(interval.stock)
  
  window_gains = seq(1, 5, 1)
  
  for (w in seq(1, 49, 12)) {
    ln_rt_stock <- diff(log(interval.stock$ltp[w:(w+11),])) - risk_free_normalised
    ln_rt_nifty <- diff(log(interval.nifty$nifty[w:(w+11)])) - risk_free_normalised
    
    capm_stock <- lm(ln_rt_stock ~ ln_rt_nifty)
    
    capm_stock_analysis<-coefficients(summary(capm_stock))
    capm_stock_analysis <- round(capm_stock_analysis,digits = 5)
    
    # rownames(capm_stock_analysis)<-c("alpha","beta")
    # capm_stock_analysis
    # print(w)
    if (capm_stock_analysis[1] > 0) {
      # print(c("long", as.numeric(interval.stock$ltp[(w + 12)]), as.numeric(interval.stock$ltp[(w + 11)])))
      window_gains[((w - 1)/12 + 1)] = as.numeric(interval.stock$ltp[(w + 12)]) - as.numeric(interval.stock$ltp[(w + 11)])
    } else {
      # print(c("short", as.numeric(interval.stock$ltp[(w + 12)]), as.numeric(interval.stock$ltp[(w + 11)])))
      window_gains[((w - 1)/12 + 1)] = as.numeric(interval.stock$ltp[(w + 11)]) -  as.numeric(interval.stock$ltp[(w + 12)])
    }
  }
  
  daily_gains[day] <- sum(window_gains)
#     }
  }
  if ((stock_id != 10) & (stock_id != 11) & (stock_id !=  14) & (stock_id != 15)) {
    print(c(stock_id, which(daily_gains > 0)))
  } 
  stock_gains[stock_id] <- sum(daily_gains)
}

stock_gains[c(-10, -11, -14, -15)]

