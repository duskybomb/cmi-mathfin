library(tseries)
library(zoo)
source("download_data.R")

## 2019 Lok Sabha elections

sector <- c("AUTO - MARUTI", "BANK - SBI", "ENERGY - RELIANCE", "FMCG - ITC", "IT - INFOSYS", "MEDIA - ZEE ENTERTAINMENT", "METAL - TATA STEEL", "PHARMA - DR. REDDY'S", "REALTY - DLF", "NIFTY 50")
file_name <- "sectors.csv"
full_data <- download_data(file_name, start_date = "2019-01-02", end_date = "2019-05-31")
date <- zoo(c(1:length(full_data[[1]])),as.Date(index(full_data[[1]])))

# Estimation window - Jan 2 to Apr 10
estim_data <- list()
for (i in seq(1,length(sector),1))
{
     estim_data[[i]] <- full_data[[i]][c(date[as.Date("2019-01-02")]:date[as.Date("2019-04-10")])]
}   

# Event window - Apr 11 - May 24
event_data <- list()
for (i in seq(1,length(sector),1))
{
  event_data[[i]] <- full_data[[i]][c(date[as.Date("2019-04-11")]:date[as.Date("2019-05-24")])]
}   

# Post-event window - May 25 - May 31
post_event_data <- list()
for (i in seq(1,length(sector),1))
{
  post_event_data[[i]] <- full_data[[i]][c(date[as.Date("2019-05-27")]:date[as.Date("2019-05-31")])]
}   


#data <- list()
#for(i in seq(1,length(sector),1))
#{
#  data[[i]] <- c(pre_data[[i]],post_data[[i]])
#}


##plot

## NIFTY Sector wise
sector <- c("AUTO - MARUTI", "BANK - SBI", "ENERGY - RELIANCE", "FMCG - ITC", "IT - INFOSYS", "MEDIA - ZEE ENTERTAINMENT", "METAL - TATA STEEL", "PHARMA - DR. REDDY'S", "REALTY - DLF", "NIFTY 50")
par(mfrow=c(5,2))
for(i in seq(1,length(sector),1))
{
  ymin = min(data[[i]])
  ymax = max(data[[i]])
  plot(data[[i]], xlab =" ", ylab="Closing price", main = sector[i], lwd = 2, col = "red")
  abline(v = c(index(data[[i]][5]),index(data[[i]][28]),index(data[[i]][31])))
}
plot(data[[2]])

## Estimation of parameters alpha & beta
Rf_rate = 3.5 ##3.5% SBI
Rf<-Rf_rate/(100*250)
ln_rt_estim_data <- capm_estim_data <- list()
apha <- beta <- vector()
for(i in seq(1,length(sector),1))
{
  ln_rt_estim_data[[i]] <- diff(log(estim_data[[i]]))-Rf
}
for(i in seq(1,length(sector),1))
{
  capm_estim_data[[i]] <- lm(ln_rt_estim_data[[i]] ~ ln_rt_estim_data[[10]])
  apha[i] <- capm_estim_data[[i]]$coefficients[1]
  beta[i] <- capm_estim_data[[i]]$coefficients[2]
}
i=10
plot(ln_rt_estim_data[[10]],ln_rt_estim_data[[i]])
abline(capm_estim_data[[i]], col = 'blue')

## Abnormal returns 
# Event window
ln_rt_event_data <- exprt_event <- AR <- list()
ln_rt_event_data[[10]] <- diff(log(event_data[[10]]))
for(i in seq(1,length(sector),1))
{
  ln_rt_event_data[[i]] <- diff(log(event_data[[i]]))
  exprt_event[[i]] <- Rf + apha[i] + beta[i]*(ln_rt_event_data[[10]]-Rf)
  AR[[i]] <- ln_rt_event_data[[i]] - exprt_event[[i]]
}
plot(AR[[i]])
## Cumulative abnormal returns
CAR <- rep(list(c(rep(0,length(index(event_data[[1]]))-1))),length(sector))
for(i in seq(1,length(sector),1))
{
  CAR[[i]] <- cumsum(AR[[i]])
}
i=10
plot(CAR[[i]])


## Abnormal returns
Rf_rate = 3.5 ##3.5% SBI
Rf<-c(rep((Rf_rate/(100*250)),length(index(data[[1]]))))
ln_rt_data <- capm_data <- list()
for(i in seq(1,length(data),1))
{
ln_rt_data[[i]] <- diff(log(data[[i]]))-Rf[2:length(data[[i]])]
}
e <- list()

for(i in seq(1,length(sector),1))
{
capm_data[[i]] <- lm(ln_rt_data[[i]] ~ ln_rt_data[[10]])
CAR[i] <- sum(capm_data[[i]]$residuals)
}

i=1
plot(ln_rt_data[[10]],ln_rt_data[[i]])
abline(capm_data[[i]], col = 'blue')



