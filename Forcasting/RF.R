library(xts)
print(load('DATA/BLOCK2/demo_intradaydata.rda'))
# class(data.stock.cash)
# str(data.stock.cash)
# class(data.stock.cash$TCS.cash)
# str(data.stock.cash$TCS.cash)
# attributes(data.stock.cash$TCS.cash[[1]])$index
# plot(y=data.stock.cash[[1]][[1]][, 'ltp'], x=data.stock.cash[[1]][[1]][, 'bsp1'])
# head(data.stock.cash[[1]][[1]][,])
# plot(data.stock.cash[[1]][[1]][which(data.stock.cash[[1]][[1]][,'ltq']>1500), 'ltp'])

# source("DATA/BLOCK2/functions.R")
# tcs.values <- intervalData(name = "TCS",
#                              instrument = 'cash', interval = "5")
# ltp <- tcs.values[[1]][,'ltp']
# bbp <- tcs.values[[1]][,'bbp1']
# bsp <- tcs.values[[1]][,'bsp1']
# all <- cbind(ltp, bbp)
# all <- cbind(all, bsp)
# plot (as.zoo(all), type = "l", plot.type="single", col=c("red", "blue", "green"))

tcs.values <- data.stock.cash[['TCS.cash']]
# plot(tcs.values[[1]][,'ttq'])
# head(tcs.values[[1]][, c('ltq', 'ttq')])
# na.omit(tcs.values[[1]][, c('ltp', 'ttq')])
vector_ltp <- as.vector(tcs.values[[1]][, c('ltp', 'ttq')])
# tcs.values[[1]][, c('ltp', 'ttq')]
# length((vector_ltp))
# vector_ltp[length(vector_ltp)/2]
# ttq[1]
# ttq[225418]
# vector_ltp[length(vector_ltp)]
# j<-0
# x<-0
# for (i in ttq) {
#   x += 1
#   if (i == j) {
#     ttq<-ttp[-x]
#     ltp<-ltp[-x]
#     x -= 1
#   }
#   else if (i - j != 1){
#     y = (ltp[x] - ltp[x-1])/
#   }
# }
# vector_ltp[length(vector_ltp)]
# 
# ltp[1]
# ltp[225419]

ltp <- vector_ltp[seq(1, length(vector_ltp)/2, 1)]
ttq <- vector_ltp[seq(length(vector_ltp)/2+1, length(vector_ltp), 1)]
ltp <- na.omit(ltp)
ttq <- na.omit(ttq)
# j <- 1
# for (i in seq(2, ttq[length(ttq)], 1)) {
#   ttq_diff <- ttq[i] - ttq[j]
#   if (is.na(ttq_diff)) {
#     print(c(i, j, ttq_diff, ttq[i], ltp[i]))
#     j<-i
#   }
#   else if (ttq_diff == 0) {
#     print(c(i, j,ttq_diff, ttq[i], ltp[i]))
#     ttq <- ttq[-i]
#     ltp <- ltp[-i]
#     i = i- 1
#     j = j -1
#   }
#   else if (ttq_diff != 1) {
#     print(c(i, j,ttq_diff, ttq[i], ltp[i]))
#     y = (ltp[i] - ltp[j])/(ttq_diff)
#     tt <- ttq[i]
#     for (x in seq(1, ttq_diff, 1)) {
#       append(ltp, ltp[j] + y*x, j+x-1)
#       append(ttq, ttq[j] + x, j+x-1)
#       i <- tt
#       j <- i
#     }
#   }
#   else {
#     j <- i
#   }
# }
ltp<- ltp[-which(duplicated(ttq))]
ttq <- ttq[!duplicated(ttq)]
par(mfrow=c(2,1))
plot(x=ttq, y=ltp, type='l')
plot(data.stock.cash[['TCS.cash']][[1]][,'ltp'])

df <- data.frame(cbind(ltp, ttq))
m <- lm(df$ltp ~ df$ttq)
summary(m)
# par(mfrow=c(2,2))
plot(df$ltp ~ df$ttq, type='l')
abline(m)
df_ltp <- zoo(ltp)
df_ltp <- diff(log(df_ltp))
d <- df_ltp
for (i in 1:5) {
  d <- cbind(d, lag(df_ltp, -i))
}
colnames(d) <- c("r",paste("r.l",1:5,sep=""))
d <- na.omit(d)
d <- as.data.frame(d)
library(randomForest)
NROW(d)
m <- randomForest(r ~ r.l1 + r.l2 + r.l3 + r.l4 + r.l5, data=d[1:13000,])
predictions <- predict(m, newdata=d[13001:13699,])
actuals <- as.numeric(d[13001:13699,1])
together <- cbind(predictions, actuals)
print(together)
cor(predictions, actuals, method="spearman")
plot(as.zoo(together), plot.type = 'single', col=c('red', 'blue'))

###########################################################################
library(randomForest)
for (k in seq(1, 15, 1)) {
  tcs.values <- data.stock.cash[[k]][[1]][, 'ltp']
  tcs.values <- diff(log(tcs.values))
  d <- tcs.values
  for (i in 1:5) {
    d <- cbind(d, lag(tcs.values, -i))
  }
  
  colnames(d) <- c("r",paste("r.l",1:5,sep=""))
  d <- na.omit(d)
  d <- as.data.frame(d)
  # write.csv(d, file="tcs1_with_lag.csv")
  x <- NROW(d)
  x_2000 <- x - 2000
  x_2000_1 <- x - 1999
  m <- randomForest(r ~ r.l1 + r.l2 + r.l3 + r.l4 + r.l5, data=d[1:x_2000,])
  predictions <- predict(m, newdata=d[x_2000_1:x,])
  actuals <- as.numeric(d[x_2000_1:x,1])
  together <- 100*cbind(predictions, actuals)
  # print(together)
  cor(predictions, actuals, method="spearman")
  #plot(as.zoo(together), plot.type = 'single', col=c('red', 'blue'))
  
  # data.stock.cash[['TCS.cash']][[1]][, 'ltp']
}
