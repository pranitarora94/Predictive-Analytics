setwd("C:/Users/ANAND/Desktop/PA/project/predictions")

library(zoo)
library(fpp)

rm(list=ls())
tsd=read.csv("matrixdf.csv")
id_data=read.csv("SubRowsEdit.csv")
#tsd1=read.csv("matrixdf.csv")
for(i in 1:length(tsd)){
  
  tsd[,i]=na.locf(tsd[,i],fromLast = "True")
}

#tsdcol4 = ts(tsd[,4], frequency=1, start=c(1972), end=c(2007))
#plot(tsdcol4)
p2008=0
p2012=0
p2007=0
p2006=0
p2005=0
p2004=0
p2003=0
p2002=0

for (i in 2:length(tsd)){
  
  tr_data=tsd[,i]
  na_cnt=sum(is.na(tr_data))
  print(na_cnt)
  
  if (na_cnt>30){
    p2007 <- tsd[,i][36]
    p2006 <- tsd[,i][35]
    p2005 <- tsd[,i][34]
    p2004 <- tsd[,i][33]
    p2003 <- tsd[,i][32]
    p2002 <- tsd[,i][31]
    
    if (!is.na(p2002) && is.na(p2003) && is.na(p2004) && is.na(p2005) && is.na(p2006)){
      slope<-(p2007-p2002)/5
      
      p2008<-p2007+slope
      p2012<-p2007+5*slope
      
    }
    else if(!is.na(p2003) && is.na(p2004) && is.na(p2005) && is.na(p2006)){
      slope<-(p2007-p2003)/4
      
      p2008<-p2007+slope
      p2012<-p2007+5*slope
      
    }
    else if(!is.na(p2004) && is.na(p2005) && is.na(p2006)){
      slope<-(p2007-p2004)/3
      
      p2008<-p2007+slope
      p2012<-p2007+5*slope
      
    }
    else if(!is.na(p2005) && is.na(2006)){
      slope<-(p2007-p2005)/2
      
      p2008<-p2007+slope
      p2012<-p2007+5*slope
      
    }
    else if(!is.na(p2006)){
      slope<-p2007-p2006
      
      p2008<-p2007+slope
      p2012<-p2007+5*slope
    }
    else
    {
      p2008<-p2007
      p2012<-p2007
    }
    'else{
    #naivets=naive(tsd[,i],h=5)
    p2008=naivets$mean[1]
    p2012=naivets$mean[5]
  }'
  #This should appear where SES is present 
  }
  else{
    rwfts=rwf(tsd[,i],h=5,drift=TRUE)
    p2008=rwfts$mean[1]
    p2012=rwfts$mean[5]
  }
  'else{
    hwts=HoltWinters(tsd[,i],h=5)
    p2008=hwts$mean[1]
    p2012=hwts$mean[5]
  }'
  'else{
  sests=ses(tsd[,i])
  p2008=sests$mean[1]
  p2012=sests$mean[5]
  
}'

  id_data[i-1,2:3]=c(p2008,p2012)
  }

write.csv(id_data,"RWF_HandleMissingVal_FilledBy_LOCF.csv")

#fitrwf2=rwf(tsdcol2,h=5,drift=TRUE)


'
Sample code for exponential smoothing

fit1 <- ses(oildata, alpha=0.2, initial="simple", h=3)
fit2 <- ses(oildata, alpha=0.6, initial="simple", h=3)
fit3 <- ses(oildata, h=3)
plot(fit1, plot.conf=FALSE, ylab="Oil (millions of tonnes)",
xlab="Year", main="", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"),
c("data", expression(alpha == 0.2), expression(alpha == 0.6),
expression(alpha == 0.89)),pch=1)
'