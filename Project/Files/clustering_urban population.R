setwd("C:/Users/ANAND/Desktop/PA/project/new")

library(cluster)
library(rworldmap)
library(HSAUR)
library(zoo)
ger=read.csv("urbanpop.csv")
str(ger)
ger$Country.Code=NULL
ger$Series.Name=NULL
ger$Series.Code=NULL
ger$X2015..YR2015.=NULL
ger$X2016..YR2016.=NULL
'ind <- which(is.na(ger), arr.ind=TRUE)
ger[ind] <- rowMeans(ger[2:16],  na.rm = TRUE)[ind[,1]]'

ger=ger[complete.cases(ger),]
for(i in 1:nrow(ger)){
  #print(i) 
  a=c(ger[i,3:16])
  a=na.locf(a)
  a=t(a)
  ger[i,3:16]=a
}
'calculate the row means to replace missing data'
#ger[2:14][which(is.na(ger[2:14]), arr.ind=TRUE)] <- rowMeans(ger[2:14][!complete.cases(ger[2:14]), ], na.rm=TRUE)
#ger=na.omit(ger)

#ind <- which(is.na(ger[2:14]), arr.ind=TRUE)
#ger[ind] <- rowMeans(ger[2:14],  na.rm = TRUE)[ind[,1]]

View(ger)

gerdist=dist(ger[2:15],method="euclidean")
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
plot ( gerkmeans$centers[i,], col = Color[i], ylim = c(0, 120), xlim = c(1,20), type = "l")
for(i in 1:12)
{
  count = count +1
  lines (gerkmeans$centers[i,], col = Color[i])
}
legend(15,120, c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9", "Cluster 10", "Cluster 11", "Cluster 12"), lty = c(1,1,1,1,1,1,1,1,1,1,1,1), col = c(Color[1], Color[2], Color[3], Color[4], Color[5], Color[6], Color[7], Color[8], Color[9], Color[10], Color[11], Color[12]),cex = 0.6)
#plot(cars, type="o", col="blue")

spdf=joinCountryData2Map(newger,joinCode = "ISO3",nameJoinColumn ="Country.Code")
mapDevice()
mapParams=mapCountryData(spdf,nameColumnToPlot = "gerkmeans.cluster",colourPalette = c(Color[1], Color[2], Color[3], Color[4], Color[5], Color[6], Color[7], Color[8], Color[9], Color[10], Color[11], Color[12]),catMethod = 'categorical',mapTitle = 'Percentage of urban population')

'
> ger$Country.Name[gerkmeans$cluster==1]
[1] American Samoa           Andorra                  Argentina               
[4] Australia                Bahamas, The             Bahrain                 
[7] Brazil                   Chile                    Curacao                 
[10] Denmark                  Finland                  Gabon                   
[13] Greenland                Israel                   Japan                   
[16] Jordan                   Korea, Rep.              Lebanon                 
[19] Luxembourg               Netherlands              New Zealand             
[22] Northern Mariana Islands Saudi Arabia             Sweden                  
[25] Turks and Caicos Islands United Arab Emirates     Venezuela, RB           
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==2]
[1] Belgium                   Bermuda                   Cayman Islands           
[4] Gibraltar                 Guam                      Hong Kong SAR, China     
[7] Iceland                   Kuwait                    Macao SAR, China         
[10] Malta                     Monaco                    Nauru                    
[13] Puerto Rico               Qatar                     San Marino               
[16] Singapore                 Sint Maarten (Dutch part) Uruguay                  
[19] Virgin Islands (U.S.)    
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==3]
[1] Burundi             Ethiopia            Liechtenstein      
[4] Malawi              Nepal               Niger              
[7] Papua New Guinea    South Sudan         Sri Lanka          
[10] Trinidad and Tobago Uganda             
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==4]
[1] Armenia                   Cabo Verde                Congo, Rep.              
[4] Ecuador                   El Salvador               Ireland                  
[7] Korea, Dem. People's Rep. Montenegro                Panama                   
[10] Poland                    Portugal                  Sao Tome and Principe    
[13] South Africa             
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==5]
[1] Belarus            Brunei Darussalam  Canada             Colombia          
[5] Cuba               Czech Republic     Djibouti           France            
[9] Germany            Greece             Libya              Mexico            
[13] Norway             Oman               Palau              Peru              
[17] Russian Federation Spain              Switzerland        United Kingdom    
[21] United States      West Bank and Gaza
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==6]
[1] Antigua and Barbuda Bangladesh          Barbados           
[4] Bhutan              Channel Islands     Comoros            
[7] Guyana              India               Lao PDR            
[10] Madagascar          Mozambique          Myanmar            
[13] St. Kitts and Nevis Sudan               Tajikistan         
[16] Tanzania            Timor-Leste         Vietnam            
[19] Yemen, Rep.         Zimbabwe           
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==7]
[1] Afghanistan           Burkina Faso          Cambodia             
[4] Chad                  Kenya                 Lesotho              
[7] Micronesia, Fed. Sts. Rwanda                Samoa                
[10] Solomon Islands       St. Lucia             Swaziland            
[13] Tonga                 Vanuatu              
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==8]
[1] Albania                        Cameroon                      
[3] China                          Cote d''Ivoire                 
[5] Fiji                           Ghana                         
[7] Guatemala                      Haiti                         
[9] Honduras                       Indonesia                     
[11] Liberia                        Slovenia                      
[13] St. Vincent and the Grenadines Turkmenistan                  
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==9]
[1] Algeria            Austria            Bolivia            Bulgaria          
[5] Costa Rica         Cyprus             Dominica           Dominican Republic
[9] Estonia            Hungary            Iran, Islamic Rep. Iraq              
[13] Italy              Latvia             Lithuania          Malaysia          
[17] Marshall Islands   Mongolia           New Caledonia      Suriname          
[21] Tunisia            Turkey             Ukraine           
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==10]
[1] Angola                   Bosnia and Herzegovina   Central African Republic
[4] Congo, Dem. Rep.         Equatorial Guinea        Grenada                 
[7] Guinea                   Kyrgyz Republic          Maldives                
[10] Mali                     Namibia                  Pakistan                
[13] Sierra Leone             Somalia                  Togo                    
[16] Uzbekistan               Zambia                  
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==11]
[1] Aruba                  Belize                 Benin                 
[4] British Virgin Islands Egypt, Arab Rep.       Faroe Islands         
[7] Guinea-Bissau          Kiribati               Mauritius             
[10] Moldova                Nigeria                Philippines           
[13] Senegal                Thailand              
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe

> ger$Country.Name[gerkmeans$cluster==12]
[1] Azerbaijan           Botswana             Croatia             
[4] French Polynesia     Gambia, The          Georgia             
[7] Isle of Man          Jamaica              Kazakhstan          
[10] Macedonia, FYR       Mauritania           Morocco             
[13] Nicaragua            Paraguay             Romania             
[16] Serbia               Seychelles           Slovak Republic     
[19] Syrian Arab Republic Tuvalu              
217 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe