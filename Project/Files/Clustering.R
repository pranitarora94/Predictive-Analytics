#setwd("C:/Users/ANAND/Desktop/PA/project/new")

library(cluster)
library(HSAUR)
ger=read.csv("Child_mortality.csv")
str(ger)
ger$Country.Code=NULL
ger$Series.Name=NULL
ger$Series.Code=NULL
#ger$X2015..YR2015. = NULL
ger$X2016..YR2016. = NULL
'ind <- which(is.na(ger), arr.ind=TRUE)
ger[ind] <- rowMeans(ger[2:16],  na.rm = TRUE)[ind[,1]]'

'calculate the row means to replace missing data'
ger[2:15][which(is.na(ger[2:15]), arr.ind=TRUE)] <- rowMeans(ger[2:15][!complete.cases(ger[2:15]), ], na.rm=TRUE)
ger=na.omit(ger)

View(ger)

gerkmeans=kmeans(ger[2:15],10)
gerkmeans$cluster
gerkmeans$size

newger=data.frame(ger,gerkmeans$cluster)
write.csv(newger,"MortalityClustered.csv")

dissE <- daisy(ger[2:15])
dE2   <- dissE^2
sk2   <- silhouette(gerkmeans$cl, dE2)

#color = ["red", "blue", "black"]
Color = palette(rainbow(10))
#plot(sk2)
tger= t(ger)
print(gerkmeans$centers)
count = 0
#plot(tger[i, ] )
i = 1
plot ( gerkmeans$centers[i,], col = Color[i], ylim = c(0, 200), xlim = c(1,15), type = "l")
for(i in 2:10)
{
  count = count +1
  lines (gerkmeans$centers[i,], col = Color[i])
}
legend(12, 200, c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9", "Cluster 10"), lty = c(1,1,1,1,1,1,1,1,1,1), col = c(Color[1], Color[2], Color[3], Color[4], Color[5], Color[6], Color[7], Color[8], Color[9], Color[10]),bty = "n")
#plot(cars, type="o", col="blue")

'
legend(12, 190, c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9", "Cluster 10"), lty = c(1,1,1,1,1,1,1,1,1,1), col = c(Color[1], Color[2], Color[3], Color[4], Color[5], Color[6], Color[7], Color[8], Color[9], Color[10]))'
'
> ger$X...Country.Name[gerkmeans$cluster==1]
[1] Comoros     Gambia, The Lao PDR     Mauritania  Pakistan    Sudan       Togo       
[8] Zimbabwe   
217 Levels: Afghanistan Albania Algeria American Samoa Andorra Angola ... Zimbabwe
> ger$X...Country.Name[gerkmeans$cluster==2]
[1] Albania                        Argentina                      Armenia                       
[4] Bahamas, The                   Barbados                       Belize                        
[7] Brazil                         Bulgaria                       Cabo Verde                    
[10] China                          Colombia                       Dominica                      
[13] Ecuador                        El Salvador                    Fiji                          
[16] Georgia                        Grenada                        Honduras                      
[19] Iran, Islamic Rep.             Jamaica                        Jordan                        
[22] Kazakhstan                     Libya                          Maldives                      
[25] Mauritius                      Mexico                         Moldova                       
[28] Palau                          Panama                         Paraguay                      
[31] Peru                           Romania                        Russian Federation            
[34] Samoa                          Saudi Arabia                   Seychelles                    
[37] Sri Lanka                      St. Kitts and Nevis            St. Lucia                     
[40] St. Vincent and the Grenadines Suriname                       Syrian Arab Republic          
[43] Thailand                       Tonga                          Trinidad and Tobago           
[46] Tunisia                        Turkey                         Venezuela, RB                 
[49] Vietnam                        West Bank and Gaza            
217 Levels: Afghanistan Albania Algeria American Samoa Andorra Angola ... Zimbabwe
> ger$X...Country.Name[gerkmeans$cluster==3]
[1] Afghanistan       Benin             Burkina Faso      Burundi           Cameroon         
[6] Congo, Dem. Rep.  Cote d\'Ivoire     Equatorial Guinea Guinea            Guinea-Bissau    
[11] Lesotho           Mozambique        South Sudan      
217 Levels: Afghanistan Albania Algeria American Samoa Andorra Angola ... Zimbabwe
> ger$X...Country.Name[gerkmeans$cluster==4]
[1] Andorra                Antigua and Barbuda    Australia              Austria               
[5] Bahrain                Belarus                Belgium                Bosnia and Herzegovina
[9] Brunei Darussalam      Canada                 Chile                  Costa Rica            
[13] Croatia                Cuba                   Cyprus                 Czech Republic        
[17] Denmark                Estonia                Finland                France                
[21] Germany                Greece                 Hungary                Iceland               
[25] Ireland                Israel                 Italy                  Japan                 
[29] Korea, Rep.            Kuwait                 Latvia                 Lebanon               
[33] Lithuania              Luxembourg             Macedonia, FYR         Malaysia              
[37] Malta                  Monaco                 Montenegro             Netherlands           
[41] New Zealand            Norway                 Oman                   Poland                
[45] Portugal               Qatar                  San Marino             Serbia                
[49] Singapore              Slovak Republic        Slovenia               Spain                 
[53] Sweden                 Switzerland            Ukraine                United Arab Emirates  
[57] United Kingdom         United States          Uruguay               
217 Levels: Afghanistan Albania Algeria American Samoa Andorra Angola ... Zimbabwe
> ger$X...Country.Name[gerkmeans$cluster==5]
[1] Angola                   Central African Republic Chad                    
[4] Mali                     Niger                    Nigeria                 
[7] Sierra Leone             Somalia                 
217 Levels: Afghanistan Albania Algeria American Samoa Andorra Angola ... Zimbabwe
> ger$X...Country.Name[gerkmeans$cluster==6]
[1] Bangladesh            Bhutan                Bolivia               Botswana             
[5] Cambodia              Eritrea               Gabon                 India                
[9] Kiribati              Myanmar               Namibia               Nepal                
[13] Papua New Guinea      Sao Tome and Principe South Africa          Tajikistan           
[17] Turkmenistan          Uzbekistan            Yemen, Rep.          
217 Levels: Afghanistan Albania Algeria American Samoa Andorra Angola ... Zimbabwe
> ger$X...Country.Name[gerkmeans$cluster==7]
[1] Ethiopia  Liberia   Malawi    Rwanda    Swaziland Uganda    Zambia   
217 Levels: Afghanistan Albania Algeria American Samoa Andorra Angola ... Zimbabwe
> ger$X...Country.Name[gerkmeans$cluster==8]
[1] Congo, Rep. Djibouti    Ghana       Kenya       Madagascar  Senegal     Tanzania   
[8] Timor-Leste
217 Levels: Afghanistan Albania Algeria American Samoa Andorra Angola ... Zimbabwe
> ger$X...Country.Name[gerkmeans$cluster==9]
[1] Algeria                              Azerbaijan                          
[3] Dominican Republic                   Egypt, Arab Rep.                    
[5] Guatemala                            Guyana                              
[7] Indonesia                            Iraq                                
[9] Korea, Dem. People\344\363\273s Rep. Kyrgyz Republic                     
[11] Marshall Islands                     Micronesia, Fed. Sts.               
[13] Mongolia                             Morocco                             
[15] Nauru                                Nicaragua                           
[17] Philippines                          Solomon Islands                     
[19] Tuvalu                               Vanuatu                             
217 Levels: Afghanistan Albania Algeria American Samoa Andorra Angola ... Zimbabwe
> ger$X...Country.Name[gerkmeans$cluster==10]
[1] Haiti
217 Levels: Afghanistan Albania Algeria American Samoa Andorra Angola ... Zimbabwe
'