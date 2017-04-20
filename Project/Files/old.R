setwd("/Users/vipinarora/Desktop/GradSem1/Predictive\ Analytics/Project/repredictiveanalyticsproject")

#------------------------------------------------
#Code for a sample time series plot
org_data=read.csv("TrainingSet1.csv")
sub_rows_data = read.csv("SubRowsEdit.csv")


pl2_data=read.csv("matrixdf.csv")
pl = pl2_data
id_data=read.csv("SubRowsEdit.csv")

for (i in 2:ncol(pl2_data)){
  if(sum(is.na(pl2_data[,i]))==35)
  {
    j = 1
    found = FALSE
    while(!found && j<37)
    {
      if(!is.na(pl2_data[j,i]))
      {
        found = TRUE
      }
      else
      {
        j = j + 1
      }
    }
    if(j<37)
    {
      for(k in 1:36)
      {
        pl2_data[k,i] <- pl2_data[j,i]
      }
    }
  }
  else
  {
    x= pl2_data[,i]
    j=1
    while(is.na(pl2_data[j,i]))  
    {
      j= j + 1       
    }
    na.interpolation(x)
    a = x[j:36]
    slope = a[2]- a[1]
    k = j
    while(k>0)
    {
      pl2_data[k,i] = pl2_data[k+1,i]- slope
      k = k-1
    }
    while(j<37)
    {
      if(!is.na(pl2_data[j,i]))
      {
        slope = x[j] - x[j-1]
        k = j
      }
      j = j+1
    }
    while(j>k && k < 36)
    {
      pl2_data[k+1,i] = pl2_data[k,i] + slope
      k = k+1
    }
  
    pl2_data[,i] = na.interpolation(pl2_data[,i])
  }
}


write.csv(pl2_data, "No_Miss_data_df.csv")

library("forecast")
setwd("/Users/vipinarora/Desktop/GradSem1/Predictive\ Analytics/Project/repredictiveanalyticsproject/OtherPics")



for (i in 2:length(pl2_data)){
  
  x= pl[,i]
  j=1
  while(is.na(pl[j,i]))  
  {
    j= j + 1       
  }
  a = x[j:36]
  a = pl2_data[,i]
  fit1 <- auto.arima(a)
  f1=forecast(fit1,h=5)
  a_cols = length(a)
  p2007 = a[a_cols]
  p2008<-f1$mean[1]
  
  p2012<-f1$mean[5]
  slope = p2008-p2007
  if(slope>0)
  {
    p2008 <- p2007 + (p2008-p2007)*0.5
    p2012 <- p2007 + (p2012-p2007)*0.5
  }
  else
  {
    p2008 <- p2007 + (p2008-p2007)*0.65
    p2012 <- p2007 + (p2012-p2007)*0.65
  }
  
  
  id_data[i-1,2:3]<-c(p2008,p2012)
  
 # jpeg(file = paste(colnames(pl2_data)[i], '.jpeg', sep=""))
#  plot(forecast(fit1,h=6))
 # dev.off()
  
}


setwd("/Users/vipinarora/Desktop/GradSem1/Predictive\ Analytics/Project/repredictiveanalyticsproject")
write.csv(id_data, "arima_diff_flatter_0.5_0.5_0.65_0.65_sub.csv")





library(ggplot2)

s=32647
cname=as.character(org_data$Country.Name[org_data$Index==s])
sname=as.character(org_data$Series.Name[org_data$Index==s])

#p=as.character(paste0("X",s))
ggplot(pl_data,aes(pl_data$Year,pl_data$X32647))+geom_line()+ylab("MDG Change")+xlab("Year")+ggtitle(paste(cname,sname))




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

  x = pl2_data[,i]
  fit1 <- auto.arima(pl2_data[,i])

  fit1
  y <- pl2_data[,i]
  kr <- KalmanRun(pl2_data[,i], fit1$model)
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
  xstar <- x
  nd <- ndiffs(xstar)
  if(nd > 0) {
    xstar <- diff(xstar,differences=nd)
  }  
  if(nd>max)
    max = nd
  
  ystar <- y
  nd <- ndiffs(ystar)
  if(nd > 0) {
    ystar <- diff(ystar,differences=nd)
  }  
    if(nd>maxy)
    maxy = nd
  tstar <- t
  ndt <- ndiffs(tstar)
  if(ndt > 0) {
    tstar <- diff(tstar,differences=ndt)
  }  
  if(ndt>maxt) {
    maxt = ndt
  }
  fit4 = auto.arima((ystar))
  fit5 = auto.arima((tstar))
  
   p2008<-f4$mean[1]
    
    p2012<-f4$mean[5]
  #jpeg(file = paste(colnames(pl2_data)[i], '.jpeg', sep=""))
    #plot(forecast(fit1,h=6))
    #f1=forecast(fit1,h=5)
    #p2008<-f1$mean[1]
    
    #p2012<-f1$mean[5]
    #
    #if(length(id.na)>30)
    #{
    
    p2006 = x[35]
    p2007 = x[36]
      if (is.na(p2006)){
        p2008<-p2007
        p2012<-p2007
      }
      else{
        slope<-p2007-p2006
       
      }
   # }
    
    p2008 <- p2007 + slope
    p2012 <- p2007 + 5*slope
    
  id_data[i-1,2:3]<-c(p2008,p2012)
}


setwd("/Users/vipinarora/Desktop/GradSem1/Predictive\ Analytics/Project/repredictiveanalyticsproject/arima_images")
write.csv(id_data, "nsub.csv")


library("imputeTS")
#cor(t,s)
#temps = ts(s, frequency = 1, start = c(1972), end = c(2007))

#moldova 116891 to 118114
#Guinea 71687 to 72776
#Cuba 45671 to 46371
#Georgia 65044:66225

a = org_data[71686:72775,]
toWrite = t(a)
temp1 = toWrite[2:37,]
write.csv(temp1, "Cuba3.csv")
my_data2 = read.csv("Cuba3.csv")
nseries = ncol(my_data2)
myData = my_data2[,2:nseries]




for (i in 1:length(myData)){
  a = myData[,i]
  if(sum(is.na(a))==35)
  {
    j = 1
    found = FALSE
    while(!found && j<37)
    {
      if(!is.na(a[j]))
      {
        found = TRUE
      }
      else
      {
        j = j + 1
      }
    }
    for(k in 1:36)
    {
      myData[k,i] <- myData[j,i]
    }
  }
  else
  {
    myData[,i] = na.interpolation(myData[,i])
  }
}

write.csv(myData, "Final_Cuba.csv")


nseries = ncol(myData)
nrows = nrow(myData)
lag_corr_mat = matrix(-1,nseries, nseries)
for (i in 1:nseries)
{
  for (j in 1:nseries)
  {
    if(i!= j)
    {
      original = myData[2:nrows, i]
      shifted = myData[1:nrows-1 , j]
    }
    
    lag_corr_mat[i,j] = cor(original, shifted)
    if(is.na(lag_corr_mat[i,j]))
    {
      lag_corr_mat[i,j] =0
    }
  }
}

for (i in 1:nseries)
{
  for (j in 1:nseries)
  {
    if(is.na(lag_corr_mat[i,j]))
    {
      lag_corr_mat[i,j] =0
    }
  }
}

to_predict_ix = myData$X65583
for (i in 1:nseries)
{
  if(myData[,i] ==  myData$X65206)
  {
    break
  }
}
max = 0
k = 0
for (j in 1:nseries)
{
  if(abs(lag_corr_mat[i,j] ) > max)
  {
    max = abs(lag_corr_mat[i,j])
    k = j
  }
}
j_max = k



my_data2 = read.csv("Moldova3.csv")
nseries = ncol(my_data2)
myData = my_data2[,2:nseries]

i = 951
k = 990
slope<-myData[36,i] - myData[35,i]
slopeK <- myData[36,k] - myData[35,k]
prev_slopeK <- myData[35,k] - myData[34,k]
slope_ratio = slopeK/prev_slopeK




org_data$Country.Name[49000]
