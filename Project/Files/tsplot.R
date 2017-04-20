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

p2007<-0
p2006<-0
p2008<-0
p2012<-0
slope<-0

for (i in 2:length(pl2_data)){
  p2007 <- pl2_data[,i][36]
  
  p2006 <- pl2_data[,i][35]
  
  if (is.na(p2006)){
    p2008<-p2007
    p2012<-p2007
  }
    else{
      slope<-p2007-p2006
      
      p2008<-p2007+slope
      p2012<-p2007+5*slope
    }
  
  id_data[i-1,2:3]<-c(p2008,p2012)
}

