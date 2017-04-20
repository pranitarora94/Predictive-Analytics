setwd("C:/Users/ANAND/Desktop/PA/project/predictions/images")

rm(list=ls())
tsd=read.csv("matrixdf.csv")

for(i in 2:length(tsd)){
  
  tsdcol4 = ts(tsd[,i], frequency=1, start=c(1972), end=c(2007))
  jpeg(file = paste(colnames(tsd)[i], '.jpeg',sep=""))
  plot(tsdcol4)
  dev.off()
  #tsd[,i]=na.locf(tsd[,i],fromLast = "True")
}