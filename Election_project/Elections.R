library(tseries)
library(zoo)
setwd("C:/Users/Navneeth Krishna/Desktop/MathFin_CMI/Election_project")

##NIFTY 50
read_data<-read.csv(file="Nifty_50_Jan-May'19.csv")
read_data
data<-as.list(read_data)
data$Date<-as.character(data$Date)
data$Date<-as.Date(data$Date,format = "%m/%d/%Y")
data$Date
close = zoo(data$Close, data$Date)
open = zoo(data$Open, data$Date)
ymin = min(data$Close)
ymax = max(data$Close)
plot(close, xlab =" ", ylab="Index", main = "NIFTY 50", lwd = 2, col = "red")
lines(open, lwd = 2, col = "blue")
abline(h = c(ymin,ymax), v = c(index(close[71]),index(close[94]),index(close[97])))
legend("topleft",legend = c("Open", "Close"), col = c("blue","red"), pch = c(19,19))
## Elections phase 1 - 11th Apr, Last phase - 19th May (Market - 20th May), Results - 23rd May

## NIFTY Sector wise
sector <- c("NIFTY AUTO", "NIFTY BANK", "NIFTY FINANCIAL SERVICES", "NIFTY FMCG", "NIFTY IT", "NIFTY MEDIA", "NIFTY METAL", "NIFTY PHARMA", "NIFTY PSU BANK", "NIFTY PVT BANK", "NIFTY REALTY")
sector_data<- list(NULL)
sector_close<-list(NULL)
sector_open<-list(NULL)
par(mfrow=c(4,3))
for(i in seq(1,length(sector),1))
{

read_data<-read.csv(file=paste0(sector[i],".csv"))
read_data<-as.list(read_data)
sector_data[[i]]<-read_data
sector_data[[i]]$Date<-as.character(sector_data[[i]]$Date)
sector_data[[i]]$Date<-as.Date(sector_data[[i]]$Date,format = "%d-%b-%Y")
sector_close[[i]] <- zoo(sector_data[[i]]$Close, sector_data[[i]]$Date)
sector_open[[i]] <- zoo(sector_data[[i]]$Open, sector_data[[i]]$Date)
ymin = min(sector_close[[i]])
ymin = min(sector_close[[i]])
plot(sector_close[[i]], xlab =" ", ylab="Index", main = sector[i], lwd = 2, col = "red")
lines(sector_open[[i]], lwd = 2, col = "blue")
abline(v = c(index(sector_close[[i]][71]),index(sector_close[[i]][94]),index(sector_close[[i]][97])))
#legend("topleft",legend = c("Open", "Close"),col = c("blue","red"), pch = c(19,19))
}

#Seperate plots
## NIFTY Sector wise
sector <- c("NIFTY AUTO", "NIFTY BANK", "NIFTY FINANCIAL SERVICES", "NIFTY FMCG", "NIFTY IT", "NIFTY MEDIA", "NIFTY METAL", "NIFTY PHARMA", "NIFTY PSU BANK", "NIFTY PVT BANK", "NIFTY REALTY")
sector_data<- list(NULL)
sector_close<-list(NULL)
sector_open<-list(NULL)
par(mfrow=c(2,2))
for(i in seq(1,4,1))
{
  
  read_data<-read.csv(file=paste0(sector[i],".csv"))
  read_data<-as.list(read_data)
  sector_data[[i]]<-read_data
  sector_data[[i]]$Date<-as.character(sector_data[[i]]$Date)
  sector_data[[i]]$Date<-as.Date(sector_data[[i]]$Date,format = "%d-%b-%Y")
  sector_close[[i]] <- zoo(sector_data[[i]]$Close, sector_data[[i]]$Date)
  sector_open[[i]] <- zoo(sector_data[[i]]$Open, sector_data[[i]]$Date)
  ymin = min(sector_close[[i]])
  ymin = min(sector_close[[i]])
  plot(sector_close[[i]], xlab =" ", ylab="Index", main = sector[i], lwd = 2, col = "red")
  lines(sector_open[[i]], lwd = 2, col = "blue")
  abline(v = c(index(sector_close[[i]][71]),index(sector_close[[i]][94]),index(sector_close[[i]][97])))
  #legend("topleft",legend = c("Open", "Close"),col = c("blue","red"), pch = c(19,19))
}

par(mfrow=c(2,2))
for(i in seq(5,8,1))
{
  
  read_data<-read.csv(file=paste0(sector[i],".csv"))
  read_data<-as.list(read_data)
  sector_data[[i]]<-read_data
  sector_data[[i]]$Date<-as.character(sector_data[[i]]$Date)
  sector_data[[i]]$Date<-as.Date(sector_data[[i]]$Date,format = "%d-%b-%Y")
  sector_close[[i]] <- zoo(sector_data[[i]]$Close, sector_data[[i]]$Date)
  sector_open[[i]] <- zoo(sector_data[[i]]$Open, sector_data[[i]]$Date)
  ymin = min(sector_close[[i]])
  ymin = min(sector_close[[i]])
  plot(sector_close[[i]], xlab =" ", ylab="Index", main = sector[i], lwd = 2, col = "red")
  lines(sector_open[[i]], lwd = 2, col = "blue")
  abline(v = c(index(sector_close[[i]][71]),index(sector_close[[i]][94]),index(sector_close[[i]][97])))
  #legend("topleft",legend = c("Open", "Close"),col = c("blue","red"), pch = c(19,19))
}

par(mfrow=c(2,2))
for(i in seq(9,11,1))
{
  
  read_data<-read.csv(file=paste0(sector[i],".csv"))
  read_data<-as.list(read_data)
  sector_data[[i]]<-read_data
  sector_data[[i]]$Date<-as.character(sector_data[[i]]$Date)
  sector_data[[i]]$Date<-as.Date(sector_data[[i]]$Date,format = "%d-%b-%Y")
  sector_close[[i]] <- zoo(sector_data[[i]]$Close, sector_data[[i]]$Date)
  sector_open[[i]] <- zoo(sector_data[[i]]$Open, sector_data[[i]]$Date)
  ymin = min(sector_close[[i]])
  ymin = min(sector_close[[i]])
  plot(sector_close[[i]], xlab =" ", ylab="Index", main = sector[i], lwd = 2, col = "red")
  lines(sector_open[[i]], lwd = 2, col = "blue")
  abline(v =_close[[i]][71]),index(sector_close[[i]][94]),index(sector_close[[i]][97])))
  #legend("topleft",legend = c("Open", "Close"),col = c("blue","red"), pch = c(19,19))
}

##Abnormal returns
Rf_rate = 3.5 ##3.5% SBI
Rf<-c(rep((Rf_rate/(100*250)),length(data$Date)))
ln_rt_Nifty <-diff(log(close))-Rf[2:length(data$Date)] ##NIFTY
##Sector wise
ln_rt_sector_data<-list()
for(i in seq(1,length(sector),1))
{
  ln_rt_sector_data[[i]] <- zoo(diff(log(sector_data[[i]]$Close))-Rf[2:length(sector_data[[i]]$Date)],sector_data[[i]]$Date[2:length(sector_data[[i]]$Date)])
}
plot()
plot(ln_rt_Nifty,ln_rt_sector_data[[1]])
plot(ln_rt_Nifty, xlab =" ", ylab="Returns", main = "NIFTY 50", lwd = 2, col = "red")
abline(v = c(index(close[71]),index(close[94]),index(close[97])))
plot(ln_rt_sector_data[[1]], xlab =" ", ylab="Returns", main = "NIFTY Auto", lwd = 2, col = "red")
abline(v = c(index(close[71]),index(close[94]),index(close[97])))
plot(ln_rt_Nifty,ln_rt_sector_data[[2]])
capm_Nifty_auto<-lm(ln_rt_sector_data[[2]]~ln_rt_Nifty)
abline(capm_Nifty_auto, col = 'blue')
summary(capm_Nifty_auto)
length(ln_rt_Nifty)
