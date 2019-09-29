setwd("/home/pruthvi/Desktop/Sem5/Data Analytics/Assignments/1st/120-years-of-olympic-history-athletes-and-results")


library(dplyr)
dataset <- read.csv("athlete_events.csv",TRUE,",")

my_set <- subset(dataset,dataset$Team=="India" & (dataset$Medal=="Gold" | dataset$Medal=="Silver" | dataset$Medal=="Bronze") , select = c(Year,Medal))


my_set <- arrange(my_set,Year)
#my_set <- count(my_set,Year)
years <- unlist(my_set[,1], use.names=FALSE)
medals <- unlist(my_set[,2], use.names=FALSE)
#transform(my_set,n=as.numeric(n))
hist(years)
#as.numeric(my_set[,2])


age_ht <- subset(dataset,dataset$Medal=="Gold"&dataset$Sport=="Athletics",select = c(Age,Height))
plot(age_ht$Age,age_ht$Height)
coeff <- cor(age_ht$Age,age_ht$Height,use = "complete.obs")



