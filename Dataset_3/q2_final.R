setwd("/home/pruthvi/Desktop/Sem5/Data Analytics/Assignments/3rd")

dataset <- read.csv("TNAgri.csv",TRUE,",")

library(dplyr)
library(plotly)

districts <- unique(subset(dataset,select = c(District_Name)))
seasons <-unique(subset(dataset,select = c(Season)))
crops <- unique(subset(dataset,select = c(Crop)))
crops <- array(unlist(crops,use.names = FALSE))

#head(dataset)

max_areas_per_crop <- c()

avgs <- c()
c <- 1

for(i in 1:length(crops)){
  k <- select(filter(dataset,dataset$Crop==crops[i]),Area)
  avgs[i] <- sum(k)/dim(k)[1]
  c <- c + 1
}

avgs <- sort(avgs,decreasing = TRUE)[1:8]

top_crops <- c()
its_avg <- c()
c <- 1
for(i in 1:length(crops)){
  k <- select(filter(dataset,dataset$Crop==crops[i]),Area)
  av <- sum(k)/dim(k)[1]
  if(av %in% avgs){
    top_crops[c] <- crops[i]
    its_avg[c] <- av
    c <- c+1
  }
}

df_crop_area <- data.frame("Crop" = top_crops,"Max_Area" = its_avg)

print("Crops under consideration")
print(df_crop_area$Crop)

area_over_years <- arrange(subset(dataset,dataset$Crop %in% df_crop_area$Crop,select = c(Crop,Crop_Year,Area)),Crop,Crop_Year)

c <- c()
c_y <- unique(select(area_over_years,Crop,Crop_Year))
cr <- unique(array(c_y$Crop))
yr <- unique(unlist(c_y$Crop_Year,use.names = FALSE))


area_sum <- c()
count <- 1

crs <- c()
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

area_over_years_n <- data.frame("Crop" = crs,"Year" = yrs,"Area_Used" = area_sum)


crop1 <- subset(area_over_years_n,area_over_years_n$Crop=="Rice")
plot(crop1$Year,crop1$Area_Used,xlab = "Year",ylab = "Area",main = "Rice",type="h")
plot(crop1$Year,crop1$Area_Used,xlab = "Year",ylab = "Area",main = "Rice",type="p")


crop2 <- subset(area_over_years_n,area_over_years_n$Crop=="Coconut ")
plot(crop2$Year,crop2$Area_Used,xlab = "Year",ylab = "Area",main = "Coconut",type="h")
plot(crop2$Year,crop2$Area_Used,xlab = "Year",ylab = "Area",main = "Coconut",type="p")


crop3 <- subset(area_over_years_n,area_over_years_n$Crop=="Groundnut")
plot(crop3$Year,crop3$Area_Used,xlab = "Year",ylab = "Area",main = "Groundnut",type="h")
plot(crop3$Year,crop3$Area_Used,xlab = "Year",ylab = "Area",main = "Groundnut",type="p")


#Don't consider "Pulses total" and "Total foodgrain" because they sapn for 
#only two years and it's not a single unique crop
crop4 <- subset(area_over_years_n,area_over_years_n$Crop=="Pulses total")
plot(crop4$Year,crop4$Area_Used,xlab = "Year",ylab = "Area",main = "Pulses total",type="h")
plot(crop4$Year,crop4$Area_Used,xlab = "Year",ylab = "Area",main = "Pulses total",type="p")


crop5 <- subset(area_over_years_n,area_over_years_n$Crop=="Total foodgrain")
plot(crop5$Year,crop5$Area_Used,xlab = "Year",ylab = "Area",main = "Total foodgrain",type="h")
plot(crop5$Year,crop5$Area_Used,xlab = "Year",ylab = "Area",main = "Total foodgrain",type="p")

#Therefore we consider the 6th and 7th
crop4 <- subset(area_over_years_n,area_over_years_n$Crop=="Jowar")
plot(crop4$Year,crop4$Area_Used,xlab = "Year",ylab = "Area",main = "Jowar",type="h")
plot(crop4$Year,crop4$Area_Used,xlab = "Year",ylab = "Area",main = "Jowar",type="p")

#Now "Guar seed" crop was cultivated only during 1999 ... not suitable for analysis over years
crop5 <- subset(area_over_years_n,area_over_years_n$Crop=="Guar seed")
plot(crop5$Year,crop5$Area_Used,xlab = "Year",ylab = "Area",main = "Guar seed",type="h")
plot(crop5$Year,crop5$Area_Used,xlab = "Year",ylab = "Area",main = "Guar seed",type="p")

#Considering 8th best
crop5 <- subset(area_over_years_n,area_over_years_n$Crop=="Sugarcane")
plot(crop5$Year,crop5$Area_Used,xlab = "Year",ylab = "Area",main = "Sugarcane",type="h")
plot(crop5$Year,crop5$Area_Used,xlab = "Year",ylab = "Area",main = "Sugarcane",type="p")

df_crop_area <- data.frame("Crop" = top_crops,"Max_Area" = its_avg)


area_over_years <- arrange(subset(dataset,dataset$Crop %in% df_crop_area$Crop,select = c(Crop,Crop_Year,Area)),Crop,Crop_Year)


c <- c()
c_y <- unique(select(area_over_years,Crop,Crop_Year))
cr <- unique(array(c_y$Crop))
yr <- unique(unlist(c_y$Crop_Year,use.names = FALSE))

area_sum <- c()
count <- 1
crs <- c()
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

