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

