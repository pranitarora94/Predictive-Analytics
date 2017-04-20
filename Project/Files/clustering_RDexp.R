setwd("C:/Users/ANAND/Desktop/PA/project/new")

library(cluster)
library(rworldmap)
library(HSAUR)
library(zoo)
ger=read.csv("RDexp.csv")

ger$Series.Name=NULL
ger$Series.Code=NULL

#for columns 
#ger[, -which(rowMeans(is.na(ger)) > 0.5)]

#for rows
ger=ger[-which(rowMeans(is.na(ger)) > 0.5), ]

for(i in 1:nrow(ger)){
  if (is.na(ger[i,3])){
  a=t(ger[i,3:15])
  a=na.locf(a[,1],fromLast = "True")
  a=t(a)
  ger[i,3:15]=a
  }
  else
  {
    a=t(ger[i,3:15])
    a=na.locf(a[,1])
    a=t(a)
    ger[i,3:15]=a 
  }
}

#ger=ger[complete.cases(ger),]

#Replace by rowmeans remaining NA data
temp=as.matrix(ger[3:15])
ind <- which(is.na(temp), arr.ind=TRUE)
temp[ind] <- rowMeans(temp,  na.rm = TRUE)[ind[,1]]
ger[3:15]=as.data.frame(temp)
View(ger)

gerdist=dist(ger[3:15],method="euclidean")
gerclst=hclust(gerdist,method="ward")
plot(gerclst)


gerkmeans=kmeans(ger[3:15],10)
gerkmeans$cluster
gerkmeans$size

newger=data.frame(ger,gerkmeans$cluster)
#write.csv(newger,"EnrollmentClustered.csv")

Color = palette(rainbow(10))
tger= t(ger)
print(gerkmeans$centers)
count = 0
plot(tger[i, ] )
i = 1
plot ( gerkmeans$centers[i,], col = Color[i], ylim = c(0, 4), xlim = c(1,20), type = "l")
for(i in 1:10)
{
  count = count +1
  lines (gerkmeans$centers[i,], col = Color[i])
}
legend(15,4, c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9", "Cluster 10"), lty = c(1,1,1,1,1,1,1,1,1,1), col = c(Color[1], Color[2], Color[3], Color[4], Color[5], Color[6], Color[7], Color[8], Color[9], Color[10]),cex = 0.8)
#plot(cars, type="o", col="blue")

spdf=joinCountryData2Map(newger,joinCode = "ISO3",nameJoinColumn ="Country.Code")
mapDevice()
mapParams=mapCountryData(spdf,nameColumnToPlot = "gerkmeans.cluster",colourPalette = c(Color[1], Color[2], Color[3], Color[4], Color[5], Color[6], Color[7], Color[8], Color[9], Color[10]),catMethod = 'categorical',mapTitle = 'Research and Development Expenditure (%)')

'
> ger$Country.Name[gerkmeans$cluster==1]
[1] Finland     Israel      Japan       Korea, Rep. Sweden     
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==2]
[1] Bosnia and Herzegovina Congo, Dem. Rep.       El Salvador           
[4] Guatemala              Iraq                   Kuwait                
[7] Macao SAR, China       Paraguay               Philippines           
[10] Saudi Arabia           Tajikistan             Trinidad and Tobago   
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==3]
[1] Armenia         Azerbaijan      Bermuda         Burkina Faso   
[5] Colombia        Ecuador         Georgia         Kazakhstan     
[9] Kyrgyz Republic Macedonia, FYR  Madagascar      Mongolia       
[13] Panama          Thailand       
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==4]
[1] Austria       Denmark       Germany       Iceland       United States
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==5]
[1] Bulgaria        Greece          Latvia          Malta           Poland         
[6] Serbia          Slovak Republic Tunisia         Turkey         
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==6]
[1] Argentina          Chile              Costa Rica         Cuba              
[5] Cyprus             Egypt, Arab Rep.   Iran, Islamic Rep. Mexico            
[9] Moldova            Pakistan           Romania            Uganda            
[13] Uruguay           
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==7]
[1] Brazil             Czech Republic     Estonia            Hungary           
[5] Ireland            Italy              New Zealand        Portugal          
[9] Russian Federation Spain             
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==8]
[1] China          Luxembourg     Netherlands    Norway         Slovenia      
[6] United Kingdom
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==9]
[1] Belarus              Croatia              Hong Kong SAR, China
[4] India                Lithuania            Malaysia            
[7] Montenegro           South Africa         Ukraine             
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==10]
[1] Australia Belgium   Canada    France    Singapore
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe
'