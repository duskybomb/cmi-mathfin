library(tseries)
library(zoo)
setwd("C:/Users/Navneeth Krishna/Desktop/MathFin_CMI/Election_project")
source("download_data.R")

election<-function(data_start_date,data_end_date,poll_start_date,poll_end_date,result_date){
## Lok Sabha elections
## Elections phase 1 - poll_start_date, Last phase - poll_end_date, Results - result_date

sector <- c("AUTO - MARUTI", "BANK - SBI", "ENERGY - RELIANCE", "FMCG - ITC", "IT - INFOSYS", "MEDIA - ZEE ENTERTAINMENT", "METAL - TATA STEEL", "PHARMA - DR. REDDY'S", "REALTY - DLF", "NIFTY 50")
file_name <- "sectors.csv"
full_data <- download_data(file_name, start_date = data_start_date, end_date = data_end_date)
date <- zoo(c(1:length(full_data[[1]])),as.Date(index(full_data[[1]])))

# Estimation window
estim_data <- list()
for (i in seq(1,length(sector),1))
{
  estim_data[[i]] <- full_data[[i]][c(date[as.Date(data_start_date)]:date[as.Date(poll_start_date)])]
}   

# Event window
event_data <- list()
for (i in seq(1,length(sector),1))
{
  event_data[[i]] <- full_data[[i]][c(date[as.Date(poll_start_date)]:date[as.Date(result_date)])]
}   

# Post-event window - May 25 - May 31
post_event_data <- list()
for (i in seq(1,length(sector),1))
{
  post_event_data[[i]] <- full_data[[i]][c(date[as.Date(result_date)]:date[as.Date(data_end_date)])]
}   


#data <- list()
#for(i in seq(1,length(sector),1))
#{
#  data[[i]] <- c(pre_data[[i]],post_data[[i]])
#}


# ##plot
# 
# ## NIFTY Sector wise
# sector <- c("AUTO - MARUTI", "BANK - SBI", "ENERGY - RELIANCE", "FMCG - ITC", "IT - INFOSYS", "MEDIA - ZEE ENTERTAINMENT", "METAL - TATA STEEL", "PHARMA - DR. REDDY'S", "REALTY - DLF", "NIFTY 50")
# for(i in seq(1,length(sector),1))
# { setwd("C:/Users/Navneeth Krishna/Desktop/MathFin_CMI/Election_project")
#   ymin = min(full_data[[i]])
#   ymax = max(full_data[[i]])
#   # Open png file
#   setwd("../Election_project/Election_plots/Closing_price/2019")
#   png(paste0(sector[i],".png"), width = 1000, height = 700)
#   plot(full_data[[i]], xlab =" ", ylab="Closing price", main = sector[i], lwd = 2, col = "blue")
#   abline(v = c(as.Date("2019-04-11"),as.Date("2019-05-20"),as.Date("2019-05-23")), col = "red")
#   # Close the file
#   dev.off()
# }

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
# i=10
# plot(ln_rt_estim_data[[10]],ln_rt_estim_data[[i]])
# abline(capm_estim_data[[i]], col = 'blue')

## Abnormal returns
# Estimation window
sd_estim_AR <- vector()
for(i in seq(1,length(sector),1))
{
  sd_estim_AR[i] <- sqrt(sum((capm_estim_data[[i]]$residuals)^2)/(length(capm_estim_data[[i]]$residuals)-2))
}
# Event window
ln_rt_event_data <- exprt_event <- AR <- list()
ln_rt_event_data[[10]] <- diff(log(event_data[[10]]))
for(i in seq(1,length(sector),1))
{
  ln_rt_event_data[[i]] <- diff(log(event_data[[i]]))
  exprt_event[[i]] <- Rf + apha[i] + beta[i]*(ln_rt_event_data[[10]]-Rf)
  AR[[i]] <- ln_rt_event_data[[i]] - exprt_event[[i]]
}
# plot(AR[[10]])
## Cumulative abnormal returns
CAR <- rep(list(c(rep(0,length(index(event_data[[1]]))-1))),length(sector))
for(i in seq(1,length(sector),1))
{
  CAR[[i]] <- cumsum(AR[[i]])
}
# i=1
# plot(CAR[[i]])
# abline(h=0, v = c(as.Date(poll_start_date),as.Date(poll_end_date),as.Date(result_date)), col = c('red','blue','blue','blue'))

## t-test
sd_CAR <- sqrt((length(event_data[[1]])-1)*((sd_estim_AR)^2))
t.value <- vector()
for(i in seq(1,length(sector),1))
{
  t.value[i] <- (CAR[[i]][length(CAR[[i]])])/(sd_CAR[i])
}
p.value <- 2*pt(abs(t.value), df = (length(capm_estim_data[[1]]$residuals)-2), lower.tail = FALSE)
print(p.value)

}

election(data_start_date = "2019-01-02",data_end_date = "2019-05-31",poll_start_date = "2019-05-20",poll_end_date = "2019-05-20",result_date = "2019-05-31")
  