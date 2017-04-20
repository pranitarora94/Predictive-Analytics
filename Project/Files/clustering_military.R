setwd("C:/Users/ANAND/Desktop/PA/project/new")

library(cluster)
library(rworldmap)
library(HSAUR)
library(zoo)
ger=read.csv("military.csv")
str(ger)
ger$Country.Code=NULL
ger$Series.Name=NULL
ger$Series.Code=NULL
ger$X2015..YR2015.=NULL
ger$X2016..YR2016.=NULL
'ind <- which(is.na(ger), arr.ind=TRUE)
ger[ind] <- rowMeans(ger[2:16],  na.rm = TRUE)[ind[,1]]'

'
for(i in 1:nrow(ger)){
  if (sum(is.na(ger[i,3:26]))<=12){
   
      #print(i) 
      a=ts(t(ger[i,3:26]))
      a=na.locf(a[,1],option="locf",na.remaining = "mean")
      ger[i,3:26]=a
    }
  
  else
  {
    ger[i,]=NULL
  }
}
'
for(i in 1:nrow(ger)){
  #print(i) 
  a=c(ger[i,3:16])
  a=na.locf(a)
  a=t(a)
  ger[i,3:16]=a
}
ger=ger[complete.cases(ger),]
'calculate the row means to replace missing data'
#ger[2:14][which(is.na(ger[2:14]), arr.ind=TRUE)] <- rowMeans(ger[2:14][!complete.cases(ger[2:14]), ], na.rm=TRUE)
#ger=na.omit(ger)

#ind <- which(is.na(ger[2:14]), arr.ind=TRUE)
#ger[ind] <- rowMeans(ger[2:14],  na.rm = TRUE)[ind[,1]]

View(ger)

gerdist=dist(ger[3:26],method="euclidean")
gerclst=hclust(gerdist,method="ward")
plot(gerclst)


gerkmeans=kmeans(ger[3:26],12)
gerkmeans$cluster
gerkmeans$size

newger=data.frame(ger,gerkmeans$cluster)
#write.csv(newger,"EnrollmentClustered.csv")

#dissE <- daisy(ger[2:16])
#dE2   <- dissE^2
#sk2   <- silhouette(gerkmeans$cl, dE2)

#color = ["red", "blue", "black"]
Color = palette(rainbow(12))
#plot(sk2)
tger= t(ger)
print(gerkmeans$centers)
count = 0
plot(tger[i, ] )
i = 1
plot ( gerkmeans$centers[i,], col = Color[i], ylim = c(0, 16), xlim = c(1,30), type = "l")
for(i in 1:12)
{
  count = count +1
  lines (gerkmeans$centers[i,], col = Color[i])
}
legend(25,16, c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9", "Cluster 10", "Cluster 11", "Cluster 12"), lty = c(1,1,1,1,1,1,1,1,1,1,1,1), col = c(Color[1], Color[2], Color[3], Color[4], Color[5], Color[6], Color[7], Color[8], Color[9], Color[10], Color[11], Color[12]),cex = 0.5)
#plot(cars, type="o", col="blue")

spdf=joinCountryData2Map(newger,joinCode = "ISO3",nameJoinColumn ="Country.Code")
mapDevice()
mapParams=mapCountryData(spdf,nameColumnToPlot = "gerkmeans.cluster",colourPalette = c(Color[1], Color[2], Color[3], Color[4], Color[5], Color[6], Color[7], Color[8], Color[9], Color[10], Color[11], Color[12]),catMethod = 'categorical',mapTitle = 'Military Expenditure (%)')

'
> ger$Country.Name[gerkmeans$cluster==1]
[1] Algeria            Azerbaijan         Botswana           Colombia          
[5] Greece             Kyrgyz Republic    Morocco            Namibia           
[9] Russian Federation United States     
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==2]
[1] Australia        Bolivia          Bulgaria         Chile           
[5] China            Ecuador          Egypt, Arab Rep. France          
[9] India            Korea, Rep.      Malaysia         Poland          
[13] Portugal         Uganda           United Kingdom   Uruguay         
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==3]
[1] Belarus      Norway       Peru         Philippines  South Africa Sweden      
[7] Thailand    
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==4]
[1] Cyprus    Ethiopia  Rwanda    Sri Lanka Turkey   
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==5]
[1] Albania    Romania    Seychelles
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==6]
[1] Austria            Cabo Verde         Dominican Republic Ghana             
[5] Guatemala          Indonesia          Ireland            Jamaica           
[9] Japan              Luxembourg         Malawi             Mexico            
[13] Nicaragua          Nigeria           
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==7]
[1] Bahrain           Brunei Darussalam Jordan            Lebanon          
[5] Pakistan          Singapore        
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==8]
[1] Argentina   Belgium     Canada      El Salvador Hungary     Madagascar 
[7] Mongolia    Mozambique  Paraguay    Switzerland Tanzania   
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==9]
[1] Bangladesh Costa Rica Haiti      Panama    
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==10]
[1] Oman         Saudi Arabia
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==11]
[1] Afghanistan Brazil      Cameroon    Denmark     Fiji        Finland    
[7] Germany     Italy       Kenya       Nepal       Netherlands New Zealand
[13] Senegal     Spain       Swaziland   Tunisia    
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==12]
[1] Angola Israel
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe
'