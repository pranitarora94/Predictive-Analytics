setwd("C:/Users/ANAND/Desktop/PA/project/predictions/images")

library(zoo)
library(fpp)

rm(list=ls())
tsd=read.csv("matrixdf.csv")
id_data=read.csv("SubRowsEdit.csv")
tsd=tsd[31:36,]
p2008=0
p2012=0
cnt=0
cn1=0
for (i in 2:length(tsd)){
  p2007 <- tsd[,i][6]
  p2006 <- tsd[,i][5]
  
  tr_data=tsd[,i]
  na_cnt=sum(is.na(tr_data))
  print(na_cnt)
  
  if (na_cnt>2){
    cnt=cnt+1
    if(is.na(p2006) && !is.na(p2007))
    {
      #cnt=cnt+1
      p2008=p2007
      p2012=p2007
    }
    else{

      slope=p2007-p2006
      p2008=p2007+0.55*slope
      p2012=p2007+2.5*slope
    }
    
  }
  else{
    cn1=cn1+1
  tsdcol = ts(tsd[,i], frequency=1, start=c(2002), end=c(2007))
  fitrwf=rwf(tsdcol,h=5,drift = TRUE)
  
  fitf=forecast(fitrwf)
  #jpeg(file = paste(colnames(tsd)[i], 'rwf.jpeg',sep=""))
  #plot(rwfts)
  #dev.off()
  p2008=fitf$mean[1]
  p2012=fitf$mean[5]
  
  slope1 = p2008-p2007
  slope2 = p2012 - p2007
  p2008=p2007 + 0.8*slope1
  p2012= p2007 + 0.8*slope2
  
  }
  id_data[i-1,2:3]=c(p2008,p2012)  
}

write.csv(id_data,"rwf_2002_0.8_duplicate.csv")


