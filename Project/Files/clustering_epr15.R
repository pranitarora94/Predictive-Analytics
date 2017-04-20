setwd("C:/Users/ANAND/Desktop/PA/project/new")

library(cluster)
library(rworldmap)
library(HSAUR)
library(zoo)
ger=read.csv("epr15.csv")

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
plot ( gerkmeans$centers[i,], col = Color[i], ylim = c(20, 100), xlim = c(1,20), type = "l")
for(i in 1:10)
{
  count = count +1
  lines (gerkmeans$centers[i,], col = Color[i])
}
legend(15,100, c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9", "Cluster 10"), lty = c(1,1,1,1,1,1,1,1,1,1), col = c(Color[1], Color[2], Color[3], Color[4], Color[5], Color[6], Color[7], Color[8], Color[9], Color[10]),cex = 0.8)
#plot(cars, type="o", col="blue")

spdf=joinCountryData2Map(newger,joinCode = "ISO3",nameJoinColumn ="Country.Code")
mapDevice()
mapParams=mapCountryData(spdf,nameColumnToPlot = "gerkmeans.cluster",colourPalette = c(Color[1], Color[2], Color[3], Color[4], Color[5], Color[6], Color[7], Color[8], Color[9], Color[10]),catMethod = 'categorical',mapTitle = 'Research and Development Expenditure (%)')

'
> ger$Country.Name[gerkmeans$cluster==1]
[1] Bolivia  China    Kuwait   Norway   Thailand Uganda   Vietnam 
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==2]
[1] Costa Rica           Czech Republic       Estonia             
[4] Finland              Georgia              Guam                
[7] Hong Kong SAR, China Ireland              Jamaica             
[10] Mongolia             Portugal             Slovenia            
[13] St. Lucia           
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==3]
[1] Croatia              Egypt, Arab Rep.     Greece              
[4] Italy                Moldova              Montenegro          
[7] Morocco              Namibia              Pakistan            
[10] Puerto Rico          Serbia               South Africa        
[13] Syrian Arab Republic Tunisia              Turkey              
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==4]
[1] Cambodia       Cayman Islands Faroe Islands  Iceland        Qatar         
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==5]
[1] Australia          Barbados           Bhutan             Brazil            
[5] Canada             Denmark            Ecuador            Ethiopia          
[9] Guatemala          Indonesia          Malaysia           Netherlands       
[13] New Zealand        Russian Federation Singapore          Sweden            
[17] United States     
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==6]
[1] Bosnia and Herzegovina Jordan                 Kosovo                
[4] Macedonia, FYR         West Bank and Gaza     Yemen, Rep.           
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==7]
[1] Armenia      Belgium      Bulgaria     Hungary      Malta        Poland      
[7] Saudi Arabia Spain        Sri Lanka   
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==8]
[1] Austria             Colombia            Cyprus             
[4] El Salvador         Honduras            Israel             
[7] Japan               Korea, Rep.         Kyrgyz Republic    
[10] Mexico              Nicaragua           Nigeria            
[13] Panama              Philippines         Trinidad and Tobago
[16] Ukraine             United Kingdom      Uruguay            
[19] Venezuela, RB      
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==9]
[1] Albania            Argentina          Azerbaijan         Chile             
[5] Cuba               Dominican Republic France             Germany           
[9] Latvia             Lithuania          Luxembourg         Mauritius         
[13] Romania            Slovak Republic   
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==10]
[1] Kazakhstan       Macao SAR, China Paraguay         Peru            
[5] Switzerland     
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe
'