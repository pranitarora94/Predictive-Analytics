setwd("C:/Users/ANAND/Desktop/PA/project/predictions/images")

library(zoo)
library(fpp)

rm(list=ls())
tsd=read.csv("matrixdf.csv")
id_data=read.csv("SubRowsEdit.csv")

#tsd=as.data.frame(tsd[20:36,])
tsd[19,13]=NA
p2008=0
p2012=0
p2007=0
p2006=0
p2005=0
p2004=0
p2003=0
p2002=0
cnt=0
cnt1=0
for (i in 2:length(tsd)){
  
  tr_data=tsd[,i]
  na_cnt=sum(is.na(tr_data))
  print(na_cnt)
  
  
  p2007 <- tsd[,i][36]
  p2006 <- tsd[,i][35]
  p2005 <- tsd[,i][34]
  p2004 <- tsd[,i][33]
  p2003 <- tsd[,i][32]
  p2002 <- tsd[,i][31]
  
  if (na_cnt>8){
    if(is.na(p2006) && !is.na(p2007))
    {
      #cnt=cnt+1
    p2008=p2007
    p2012=p2007
    }
    else{
      cnt1=cnt1+1
    slope=p2007-p2006
    p2008=p2007+0.55*slope
    p2012=p2007+2.5*slope
    }
  }
  
  else{
    cnt=cnt+1
    tsdcol = ts(tsd[,i], frequency=1, start=c(1972), end=c(2007))
    twin=window(tsdcol,start=2000,end=2007)
    #rwfts=rwf(tsdcol,h=5,drift=TRUE)
    #holtts=auto.arima(twin,trace=TRUE,stepwise = FALSE,start.p=)
    holtts=holt(twin,damped=TRUE,h=5,exponential = TRUE)
    holtf=forecast(holtts)
    #jpeg(file = paste(colnames(tsd)[i], 'rwf.jpeg',sep=""))
    #plot(rwfts)
    #dev.off()
    p2008=holtf$mean[1]
    p2012=holtf$mean[5]
    
    
    #slope1 = p2008-p2007
    #slope2 = p2012 - p2007
    #p2008=p2007 + 0.90*slope1
    #p2012= p2007 + 0.90*slope2
  }
id_data[i-1,2:3]=c(p2008,p2012)

}

write.csv(id_data,"Holt_optimal_1.csv")
