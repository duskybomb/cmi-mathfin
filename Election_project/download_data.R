library(tseries)
download_data<-function(file_name,start_date,end_date){
#Read data from .csv file
 read_data<-read.csv(file=file_name,header = TRUE)
 #Store stock names as an array of strings
 stock_name<-c(read_data)
 stock_name = unlist(stock_name)
 stock_name = as.character(stock_name)
 m <- length(stock_name)
 data<-list()
 for(i in 1:m){
    
    data[[i]]<- get.hist.quote(instrument = stock_name[i]
                               ,start=start_date
                               ,end=end_date
                               ,quote="AdjClose"
                               ,provider = "yahoo")
    #r = rand(1:10)
    #Sys.sleep(10) #Add a system delay
    
  }
  return(data)
}
