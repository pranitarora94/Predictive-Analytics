setwd("/Users/vipinarora/Desktop/GradSem1/Predictive\ Analytics/Project/repredictiveanalyticsproject")

#------------------------------------------------
#Code for a sample time series plot
org_data=read.csv("TrainingSet1.csv")
sub_rows_data = read.csv("SubRowsEdit.csv")

ortemp = org_data[, 1:37]
or3_data = t(ortemp)
write.csv(or5_data, "Ner_train.csv")
or4_data = read.csv("Nr_train.csv")
or5_data = or4_data
pl_data=read.csv("matrixdf.csv")
j =  2
curr_country = org_data[2,38]
  count = 0
  max_count = 0

  new_data = org_data[,2:37]
  
for (i in 1:ncol(or5_data)){
}
  
  na.interpolation( or5_data[,i]) 
  temp = org_data[i,]
  na_sum = sum(is.na(org_data[i,]))
  
  #while(org_data[j,1] != col)
  #{
  #  j <- j+1
  #}
  
  org_data[i,] <- na.interpolation(org_data[i,])
  if(org_data[i,38] == curr_country)
  {
    count <- count + na_sum
    sub_rows_data[j,2] = count 
    sub_rows_data[j,3] = curr_country
    if(count > max_count)
    {
      max_count <- count
      
    }
  }
  else
  {
    count <- na_sum
    curr_country <- org_data[i,38]
    j <- j + 1
  }
}


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
#Tajikistan
#Georgia
#Macedonia



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
        if(slope>0)
        {
          p2008<-p2007+0.6*slope
          p2012<-p2007+2.5*slope
        }
        else
        {
          p2008<-p2007+0.6*slope
          p2012<-p2007+2.5*slope
        }
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

#Cuba
#1 64192    #i 1      #k 2      #lag_corr_mat[i,k] 0.8989511
#2 64735    #i 318    #k 344    #lag_corr_mat[i,k] -0.9928111
#3 64794    #i 337    #k 162    #lag_corr_mat[i,k] -0.9963906
#4 64882    #i 400    #k 688    #lag_corr_mat[i,k] 0.9485135
#5 64929    #i 439    #k 435    #lag_corr_mat[i,k] 0.9976922
#6 65206    #i 535    #k 127    #lag_corr_mat[i,k] 0     No varience.... 127 is all zero


#Guinea
#1 104302   #i 4      #k 939    #lag_corr_mat[i,k] 0.9959625
#2 104845   #i 440    #k 626    #lag_corr_mat[i,k] 0.9984937
#3 104904   #i 495    #k 944    #lag_corr_mat[i,k] 0.9857832
#4 105039   #i 614    #k 610    #lag_corr_mat[i,k] 0.9997288
#5 105316   #i 852    #k 325    #lag_corr_mat[i,k] -0.9964885
#6 105595   #i 1080   #k 587    #lag_corr_mat[i,k] -0.8777808


#Moldova
#1 169815   #i 4      #k 781    #lag_corr_mat[i,k] 0.9215857
#2 170358   #i 514    #k 511    #lag_corr_mat[i,k] 0.9979317
#3 170417   #i 572    #k 246    #lag_corr_mat[i,k] 0.9965129
#4 170505   #i 653    #k 88     #lag_corr_mat[i,k] 0.9982229
#5 170552   #i 700    #k 696    #lag_corr_mat[i,k] 0.9965498
#6 170829   #i 951    #k 990    #lag_corr_mat[i,k] 0.9887624


#Georgia
#1 94149    #i 481    #k 478    #lag_corr_mat[i,k] -0.9993101
#2 94208    #i 540    #k 957    #lag_corr_mat[i,k] 0.9976003
#3 94296    #i 613    #k 562    #lag_corr_mat[i,k] 0.9999756
#4 94343    #i 659    #k 655    #lag_corr_mat[i,k] 0.9660165



my_data2 = read.csv("Moldova3.csv")
nseries = ncol(my_data2)
myData = my_data2[,2:nseries]

i = 951
k = 990
slope<-myData[36,i] - myData[35,i]
slopeK <- myData[36,k] - myData[35,k]
prev_slopeK <- myData[35,k] - myData[34,k]
slope_ratio = slopeK/prev_slopeK

#Moldova
#1 169815   #i 4      #k 781    #lag_corr_mat[i,k] 0.9215857    -0.0062278//1.75
#2 170358   #i 514    #k 511    #lag_corr_mat[i,k] 0.9979317    0.003//0.875
#3 170417   #i 572    #k 246    #lag_corr_mat[i,k] 0.9965129    0.0082935//1
#4 170505   #i 653    #k 88     #lag_corr_mat[i,k] 0.9982229    -2e-06//1
#5 170552   #i 700    #k 696    #lag_corr_mat[i,k] 0.9965498    -9e-04//1.142857
#6 170829   #i 951    #k 990    #lag_corr_mat[i,k] 0.9887624    0//-1.637495


old2008 = 0.0202
old2012 = 0.0166
slope = -9e-04
new_slope = slope*1.1
new2008 = old2008-slope + new_slope
new2012 = old2008 - slope + 5*new_slope
new2008
new2012



#Cuba
#1 64192    #i 1      #k 2      #lag_corr_mat[i,k] 0.8989511    0.0267091//slope_ratio   1.429352
#2 64735    #i 318    #k 344    #lag_corr_mat[i,k] -0.9928111    0.002//slope_ratio   -12.5
#3 64794    #i 337    #k 162    #lag_corr_mat[i,k] -0.9963906   0.005304//slope_ratio   1 (nan)
#4 64882    #i 400    #k 688    #lag_corr_mat[i,k] 0.9485135   -8e-05 //slope_ratio   1 
#5 64929    #i 439    #k 435    #lag_corr_mat[i,k] 0.9976922    -1e-04//slope_ratio   0.5
#6 65206    #i 535    #k 127    #lag_corr_mat[i,k] 0            0 //slope_ratio 1 (nan)    No varience.... 127 is all zero 


#Guinea
#1 104302   #i 4      #k 939    #lag_corr_mat[i,k] 0.9959625  0.0198413//0.74394
#2 104845   #i 440    #k 626    #lag_corr_mat[i,k] 0.9984937  0.01//1
#3 104904   #i 495    #k 944    #lag_corr_mat[i,k] 0.9857832  0.001425332//0.83
#4 105039   #i 614    #k 610    #lag_corr_mat[i,k] 0.9997288  -0.0052//0.8235294
#5 105316   #i 852    #k 325    #lag_corr_mat[i,k] -0.9964885 0//0.5
#6 105595   #i 1080   #k 587    #lag_corr_mat[i,k] -0.8777808 -//3.00



org_data$Country.Name[49000]
