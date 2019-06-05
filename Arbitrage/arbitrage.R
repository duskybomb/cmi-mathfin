m <- 4/365
call_money_rate <- 3.19 # 3.24
nintyone_day_t_bill <- 3.28
r_m <- call_money_rate + (nintyone_day_t_bill - call_money_rate)*4/90 # risk free rate
r_c <- m*log(1 + r_m/m)
print(load('DATA/BLOCK2/demo_intradaydata.rda'))

stock <- data.stock.cash[[x]]
stock.f <- data.stock.futures[[x]]
stock.f.day1 <- tcs.f[[1]]
spot <- stock[[1]][,"ltp"]
futures <- stock.f.day1[,"ltp"]
theoretical_future = spot*exp(r_c * t)
ncf_f1_less_f <- 