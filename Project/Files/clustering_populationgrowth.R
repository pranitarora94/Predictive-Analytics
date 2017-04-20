setwd("C:/Users/ANAND/Desktop/PA/project/new")

library(cluster)
library(rworldmap)
library(HSAUR)
library(zoo)
ger=read.csv("popgrowth.csv")
str(ger)
ger$Country.Code=NULL
ger$Series.Name=NULL
ger$Series.Code=NULL
ger$X2015..YR2015.=NULL
ger$X2016..YR2016.=NULL
'ind <- which(is.na(ger), arr.ind=TRUE)
ger[ind] <- rowMeans(ger[2:16],  na.rm = TRUE)[ind[,1]]'


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

gerdist=dist(ger[3:16],method="euclidean")
gerclst=hclust(gerdist,method="ward")
plot(gerclst)


gerkmeans=kmeans(ger[3:16],12)
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
plot ( gerkmeans$centers[i,], col = Color[i], ylim = c(-10, 18), xlim = c(1,20), type = "l")
for(i in 1:12)
{
  count = count +1
  lines (gerkmeans$centers[i,], col = Color[i])
}
legend(15,18, c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9", "Cluster 10", "Cluster 11", "Cluster 12"), lty = c(1,1,1,1,1,1,1,1,1,1,1,1), col = c(Color[1], Color[2], Color[3], Color[4], Color[5], Color[6], Color[7], Color[8], Color[9], Color[10], Color[11], Color[12]),cex = 0.6)
#plot(cars, type="o", col="blue")

spdf=joinCountryData2Map(newger,joinCode = "ISO3",nameJoinColumn ="Country.Code")
mapDevice()
mapParams=mapCountryData(spdf,nameColumnToPlot = "gerkmeans.cluster",colourPalette = c(Color[1], Color[2], Color[3], Color[4], Color[5], Color[6], Color[7], Color[8], Color[9], Color[10], Color[11], Color[12]),catMethod = 'categorical',mapTitle = 'Population Growth')

for(j in 1:nrow(ger)){
for (i in 3:length(ger)){
  
    if (ger[j,i]==-9.816607){
      print('Yes')
      break
    }
  }
}
print(j)
print(i)
'

gerkmeans$size
 [1] 36 10  2 23  2  2 31  2 34 37 37  1

> ger$Country.Name[gerkmeans$cluster==1]
[1] Belize                 Bhutan                 British Virgin Islands
[4] Cameroon               Cayman Islands         Comoros               
[7] Congo, Rep.            Cote d''Ivoire          Eritrea               
[10] Ethiopia               Gabon                  Ghana                 
[13] Guatemala              Guinea                 Guinea-Bissau         
[16] Kenya                  Liberia                Macao SAR, China      
[19] Maldives               Mauritania             Nigeria               
[22] Pakistan               Papua New Guinea       Rwanda                
[25] Sao Tome and Principe  Saudi Arabia           Singapore             
[28] Solomon Islands        Somalia                Sudan                 
[31] Tajikistan             Timor-Leste            Togo                  
[34] Vanuatu                West Bank and Gaza     Yemen, Rep.           
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==2]
[1] Albania                  American Samoa           Bulgaria                
[4] Georgia                  Latvia                   Lithuania               
[7] Northern Mariana Islands Puerto Rico              Romania                 
[10] Ukraine                 
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==3]
[1] Bahrain Kuwait 
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==4]
[1] Afghanistan              Angola                   Benin                   
[4] Burkina Faso             Burundi                  Chad                    
[7] Congo, Dem. Rep.         Equatorial Guinea        Gambia, The             
[10] Iraq                     Jordan                   Madagascar              
[13] Malawi                   Mali                     Mozambique              
[16] Niger                    Senegal                  Sierra Leone            
[19] South Sudan              Tanzania                 Turks and Caicos Islands
[22] Uganda                   Zambia                  
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==5]
[1] Andorra              Syrian Arab Republic
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==6]
[1] Qatar                United Arab Emirates
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==7]
[1] Algeria                  Australia                Bahamas, The            
[4] Bolivia                  Botswana                 Brunei Darussalam       
[7] Cambodia                 Central African Republic Ecuador                 
[10] Egypt, Arab Rep.         Haiti                    Honduras                
[13] Israel                   Kazakhstan               Kiribati                
[16] Kyrgyz Republic          Lao PDR                  Luxembourg              
[19] Malaysia                 Mexico                   Mongolia                
[22] Namibia                  New Caledonia            Panama                  
[25] Philippines              South Africa             Swaziland               
[28] Turkey                   Uzbekistan               Venezuela, RB           
[31] Zimbabwe                
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==8]
[1] Lebanon Oman   
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==9]
[1] Austria                   Belgium                   Channel Islands          
[4] China                     Denmark                   Fiji                     
[7] Finland                   France                    Guam                     
[10] Hong Kong SAR, China      Isle of Man               Italy                    
[13] Korea, Dem. People's Rep. Korea, Rep.               Kosovo                   
[16] Lesotho                   Liechtenstein             Malta                    
[19] Myanmar                   Netherlands               Norway                   
[22] Palau                     Samoa                     Seychelles               
[25] Sri Lanka                 St. Martin (French part)  Suriname                 
[28] Sweden                    Switzerland               Thailand                 
[31] Tonga                     Trinidad and Tobago       United Kingdom           
[34] United States            
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==10]
[1] Armenia                        Barbados                      
[3] Belarus                        Bermuda                       
[5] Bosnia and Herzegovina         Croatia                       
[7] Cuba                           Czech Republic                
[9] Dominica                       El Salvador                   
[11] Estonia                        Faroe Islands                 
[13] Germany                        Greece                        
[15] Greenland                      Grenada                       
[17] Guyana                         Hungary                       
[19] Jamaica                        Japan                         
[21] Macedonia, FYR                 Marshall Islands              
[23] Mauritius                      Micronesia, Fed. Sts.         
[25] Moldova                        Montenegro                    
[27] Nauru                          Poland                        
[29] Portugal                       Russian Federation            
[31] Serbia                         Slovak Republic               
[33] Slovenia                       St. Vincent and the Grenadines
[35] Tuvalu                         Uruguay                       
[37] Virgin Islands (U.S.)         
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==11]
[1] Antigua and Barbuda Argentina           Aruba              
[4] Azerbaijan          Bangladesh          Brazil             
[7] Cabo Verde          Canada              Chile              
[10] Colombia            Costa Rica          Curacao            
[13] Cyprus              Djibouti            Dominican Republic 
[16] French Polynesia    Gibraltar           Iceland            
[19] India               Indonesia           Iran, Islamic Rep. 
[22] Ireland             Libya               Monaco             
[25] Morocco             Nepal               New Zealand        
[28] Nicaragua           Paraguay            Peru               
[31] San Marino          Spain               St. Kitts and Nevis
[34] St. Lucia           Tunisia             Turkmenistan       
[37] Vietnam            
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==12]
[1] Sint Maarten (Dutch part)
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe