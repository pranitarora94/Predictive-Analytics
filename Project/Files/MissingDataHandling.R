
#------------------------------------------------
#Code for a sample time series plot
org_data=read.csv("TrainingSet1.csv")
pl_data=read.csv("matrixdf.csv")
library(ggplot2)

s=32647
cname=as.character(org_data$Country.Name[org_data$Index==s])
sname=as.character(org_data$Series.Name[org_data$Index==s])

#p=as.character(paste0("X",s))
ggplot(pl_data,aes(pl_data$Year,pl_data$X32647))+geom_line()+ylab("MDG Change")+xlab("Year")+ggtitle(paste(cname,sname))

#-------------------------------------------
#Code for baseline model
pl_data=read.csv("matrixdf.csv")
pl2_data=pl_data

id_data=read.csv("SubRowsEdit.csv")


library("forecast")

kr <- KalmanSmooth(ger$X2002..YR2002., fit1$model)
tmp <- which(fit1$model$Z == 1)
id <- ifelse (length(tmp) == 1, tmp[1], tmp[2])
id.na <- which(is.na(ger$X2002..YR2002.))
msci.filled <- ger$X2002..YR2002.
msci.filled[id.na] <- kr$smooth[id.na,id]

require("tsoutliers")
tso(msci.filled, remove.method = "bottom-up", tsmethod = "arima", 
    args.tsmethod = list(order = c(1,1,1)))
tso(msci.filled, remove.method = "bottom-up", args.tsmethod = list(ic = "bic"))


mo <- outliers("LS", 18)
ls <- outliers.effects(mo, length(msci))
fit2 <- auto.arima(ger$X2002..YR2002., xreg = ls)
fit2



p2007<-0
p2006<-0
p2008<-0
p2012<-0
slope<-0
library("tseries")

max = 0
maxy = 0
maxt = 0
for (i in 2:length(pl2_data)){
#  p2007 <- pl2_data[,i][36]
  
 # p2006 <- pl2_data[,i][35]
  x = pl2_data[,i]
  fit1 <- auto.arima(pl2_data[,i])
  #fit1 <- arima(ger$X2002..YR2002.)
  fit1
  y <- pl2_data[,i]
  #kr <- KalmanSmooth(pl2_data[,i], fit1$model)
  kr <- KalmanRun(pl2_data[,i], fit1$model)
  #tmp <- which(fit1$model$Z == 1)
  #id <- ifelse (length(tmp) == 1, tmp[1], tmp[2])
  #id.na <- which(is.na(pl2_data[,i]))
  #msci.filled <- pl2_data[,i]
  #msci.filled[id.na] <- kr$smooth[id.na,id]
  id.na <- which(is.na(pl2_data[,i]))
  for (j in id.na)
    y[j] <- fit1$model$Z %*% kr$states[j,]
    
  sapply(id.na, FUN = function(x, Z, alpha) Z %*% alpha[x,], 
         Z = fit1$model$Z, alpha = kr$states)
  y[id.na]  
  
  kr <- KalmanSmooth(pl2_data[,i], fit1$model)
  t = pl2_data[,i]
  for (j in id.na)
    t[j] <- kr$smooth[j,]
  
  plot(pl2_data[,i], ylim = c(0, 1.5), xlim = c(1,37))
  lines(y, col="blue")
  lines(t, col="red")
  #adf.test(y, alternative = "stationary") #Using 5% threshold, differencing is required if p-value > 0.05.
  #adf.test(t, alternative = "stationary")
  
#  kpss.test(x)
#  kpss.test(y)    #p-values less than 0.05 suggest that differencing is required.
#  kpss.test(t)
  
#  plot(diff(t,1), xlab="Year", ylab="Annual change in monthly log A10 sales")
  
#  temp = diff(t,4)
#  adf.test(temp, alternative = "stationary")
  
  #ns <- nsdiffs(x)
  xstar <- x
#  if(ns > 0) {
#    xstar <- diff(x,lag=frequency(x),differences=ns)
#  }
  nd <- ndiffs(xstar)
  if(nd > 0) {
    xstar <- diff(xstar,differences=nd)
  }  
#  nd
#  plot(xstar)
  if(nd>max)
    max = nd
  
 # ns <- nsdiffs(y)
  ystar <- y
#  if(ns > 0) {
#    ystar <- diff(y,lag=frequency(y),differences=ns)
#  }
  nd <- ndiffs(ystar)
  if(nd > 0) {
    ystar <- diff(ystar,differences=nd)
  }  
  
 # nd
 # plot(ystar)
  
  if(nd>maxy)
    maxy = nd
  
#  ns <- nsdiffs(t)
  tstar <- t
#  if(ns > 0) {
 #   tstar <- diff(t,lag=frequency(t),differences=ns)
#  } 
  ndt <- ndiffs(tstar)
  if(ndt > 0) {
    tstar <- diff(tstar,differences=ndt)
  }  
  
#  nd
#  plot(tstar)
  if(ndt>maxt) {
    maxt = ndt
  }
  fit4 = auto.arima((ystar))
  fit5 = auto.arima((tstar))
  
  f4 = forecast(fit5, h=5)
  if(ndt ==0)
  {
    p2008<-f4$mean[1]
    
    p2012<-f4$mean[5]
  }
  else if(ndt == 1)
  {
    val2008  = f4$mean[1] + t[36]
    val2009 = f4$mean[2] + val2008
    val2010 = f4$mean[3] + val2009
    val2011 = f4$mean[4] + val2010
    val2012  = f4$mean[5] + val2011
  
    p2008<-val2008
    p2012<-val2012
  }
  else 
  {   #assume ndt = 2
      ttemp <- diff(t,differences=1)
    
      val2008  = f4$mean[1] + ttemp[35]
      val2009 = f4$mean[2] + val2008
      val2010 = f4$mean[3] + val2009
      val2011 = f4$mean[4] + val2010
      val2012  = f4$mean[5] + val2011
      
      val2008 <- val2008 + t[36]
      val2009 <- val2009 + val2008
      val2010 <- val2010 + val2009
      val2011 <- val2011 + val2010
      val2012 <- val2012 + val2011
      p2008<-val2008
      p2012<-val2012
  }
  
  #jpeg(file = paste(colnames(pl2_data)[i], '.jpeg', sep=""))
    #plot(forecast(fit1,h=6))
    #dev.off()
    #plot(forecast(fit1,h=6))
    #fit2 = auto.arima(y)
    #plot(forecast(fit2,h=6))
    #fit3 = auto.arima(t)
    #plot(forecast(fit3,h=6))
    
    #f1=forecast(fit3,h=5)
    #p2008<-f1$mean[1]
    
    #p2012<-f1$mean[5]
    #
    #if(length(id.na)>30)
    #{
    #  if (is.na(p2006)){
    #    p2008<-p2007
    #    p2012<-p2007
    #  }
    #  else{
    #    slope<-p2007-p2006
        
    #    p2008<-p2007+slope
    #    p2012<-p2007+5*slope
    #  }
   # }
    
  id_data[i-1,2:3]<-c(p2008,p2012)
}


setwd("/Users/vipinarora/Desktop/GradSem1/Predictive\ Analytics/Project/repredictiveanalyticsproject/arima_images")
write.csv(id_data, "sub5.csv")

