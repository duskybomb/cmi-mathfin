library(tseries)
library(zoo)
setwd("C:/Users/Navneeth Krishna/Desktop/MathFin_CMI/Election_project")
source("download_data.R")

## 2019 Lok Sabha elections
## Elections phase 1 - 7th Apr, Last phase - 12th May (Market - 20th May), Results - 16th May

sector <- c("AUTO - MARUTI", "BANK - SBI", "ENERGY - RELIANCE", "FMCG - ITC", "IT - INFOSYS", "MEDIA - ZEE ENTERTAINMENT", "METAL - TATA STEEL", "PHARMA - DR. REDDY'S", "REALTY - DLF", "NIFTY 50")
file_name <- "sectors.csv"
full_data <- download_data(file_name, start_date = "2014-01-02", end_date = "2014-06-16")
date <- zoo(c(1:length(full_data[[1]])),as.Date(index(full_data[[1]])))

# Estimation window - Jan 2, 2014 to Apr 6,2014
estim_data <- list()
for (i in seq(1,length(sector),1))
{
  estim_data[[i]] <- full_data[[i]][c(date[as.Date("2014-01-02")]:date[as.Date("2014-04-04")])]
} 
# na.spline(full_data[[10]])

# Event window - May 16 - June 16 
event_data <- list()
for (i in seq(1,length(sector),1))
{
  event_data[[i]] <- full_data[[i]][c(date[as.Date("2014-05-16")]:date[as.Date("2014-06-16")])]
}  

# Post-event window - May 16 - June 16
post_event_data <- list()
for (i in seq(1,length(sector),1))
{
  post_event_data[[i]] <- full_data[[i]][c(date[as.Date("2014-05-16")]:date[as.Date("2014-06-16")])]
}  

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
Rf<-Rf_rate/250
ln_rt_estim_data <- capm_estim_data <- list()
alpha <- beta <- vector()
for(i in seq(1,length(sector),1))
{
  ln_rt_estim_data[[i]] <- 100*diff(log(estim_data[[i]]))-Rf
  
}
for(i in seq(1,length(sector),1))
{
  capm_estim_data[[i]] <- lm(ln_rt_estim_data[[i]] ~ ln_rt_estim_data[[10]])
  alpha[i] <- capm_estim_data[[i]]$coefficients[1]
  beta[i] <- capm_estim_data[[i]]$coefficients[2]
  plot(ln_rt_estim_data[[10]],ln_rt_estim_data[[i]])
  abline(capm_estim_data[[i]], col = 'blue')
}
i=1
plot(ln_rt_estim_data[[10]],ln_rt_estim_data[[i]])
abline(capm_estim_data[[i]], col = 'blue')

## Abnormal returns
# Estimation window
sd_estim_AR <- vector()
for(i in seq(1,length(sector),1))
{
  sd_estim_AR[i] <- sqrt(sum((capm_estim_data[[i]]$residuals)^2)/(capm_estim_data[[i]]$df.residual))
}
# sum <- 0
# for(j in seq(1,270,1))
# {
#  sum <- sum +  sum(capm_estim_data[[1]]$residuals[j]^2)
# }
# Event window
ln_rt_event_data <- exprt_event <- AR <- list()
ln_rt_event_data[[10]] <- diff(log(event_data[[10]]))*100
for(i in seq(1,length(sector),1))
{
  ln_rt_event_data[[i]] <- diff(log(event_data[[i]]))*100
  exprt_event[[i]] <- Rf + alpha[i] + beta[i]*(ln_rt_event_data[[10]]-Rf)
  AR[[i]] <- (ln_rt_event_data[[i]] - exprt_event[[i]])
}
i=6
plot(ln_rt_event_data[[i]],col='blue')
lines(exprt_event[[i]], col='red')
i=7
plot(AR[[i]])
## Cumulative abnormal returns
CAR <- rep(list(c(rep(0,length(index(event_data[[1]]))-1))),length(sector))
for(i in seq(1,length(sector),1))
{
  CAR[[i]] <- cumsum(AR[[i]])
}
i=6
plot(CAR[[i]])
abline(h=0, v = c(as.Date("2014-04-07"),as.Date("2014-05-14"),as.Date("2014-05-16")), col = c('red','blue','blue','blue'))

## t-test
sd_CAR <- sqrt(length(event_data[[1]])*((sd_estim_AR)^2))
t.value <- p.value <- vector()
# t <- vector()
for(i in seq(1,length(sector),1))
{
  # t[i] <- AR[[i]][length(AR[i])]/sd_estim_AR[i]
  t.value[i] <- (CAR[[i]][length(CAR[[i]])])/(sd_CAR[i])
}

p.value <- 2*pt(abs(t.value), df =(length(capm_estim_data[[1]]$residuals)-2), lower = FALSE)
p.value