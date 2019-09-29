df_crop_area <- data.frame("Crop" = top_crops,"Max_Area" = its_avg)

#subsetting area used for each of the top crops over the years
area_over_years <- arrange(subset(dataset,dataset$Crop %in% df_crop_area$Crop,select = c(Crop,Crop_Year,Area)),Crop,Crop_Year)


c <- c()
c_y <- unique(select(area_over_years,Crop,Crop_Year))
cr <- unique(array(c_y$Crop))
yr <- unique(unlist(c_y$Crop_Year,use.names = FALSE))

#Sum of area used for a crop over a year, across all the districts it was cultivated
area_sum <- c()
count <- 1
#Crop name
crs <- c()
#Corresponding years it was cultivated
yrs <- c()

for(i in 1:length(cr)){
  for(j in 1:length(yr)){
    x <- subset(area_over_years,area_over_years$Crop==cr[i] & area_over_years$Crop_Year==yr[j],select = c(Area))
    if(dim(x)[1]>0){
      area_sum[count] <- sum(x)
      crs[count] <- cr[i]
      yrs[count] <- yr[j]
      count <- count + 1
    }
  }
}


#Simplified data frame that gives Crops name , Year cultivated , Area of land used in that year 
area_over_years_n <- data.frame("Crop" = crs,"Year" = yrs,"Area_Used" = area_sum)


#plot data
crop1 <- subset(area_over_years_n,area_over_years_n$Crop=="Rice")
crop2 <- subset(area_over_years_n,area_over_years_n$Crop=="Coconut ")
crop3 <- subset(area_over_years_n,area_over_years_n$Crop=="Groundnut")
crop4 <- subset(area_over_years_n,area_over_years_n$Crop=="Jowar")
crop5 <- subset(area_over_years_n,area_over_years_n$Crop=="Sugarcane")

print(plot(crop1$Year,crop1$Area_Used,xlab = "Year",ylab = "Area",main = "Rice",type="p"))
print(plot(sort(crop2$Year),crop2$Area_Used,xlab = "Year",ylab = "Area",main = "Coconut",type="p"))
print(plot(crop3$Year,crop3$Area_Used,xlab = "Year",ylab = "Area",main = "Groundnut",type="p"))
print(plot(crop4$Year,crop4$Area_Used,xlab = "Year",ylab = "Area",main = "Jowar",type="p"))
print(plot(crop5$Year,crop5$Area_Used,xlab = "Year",ylab = "Area",main = "Sugarcane",type="p"))

plot_ly(x=crop5$Year,y=crop5$Area_Used,type = "scatter",mode = "markers") %>% 
  layout(title= "Sugarcane" , xaxis = list(title = "Year"),yaxis = list(title = "Area"))
