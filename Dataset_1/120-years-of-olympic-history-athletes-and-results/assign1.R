#The original CSV has been edited , I have added column names 'P' and 'Q' 
#to 2 columns following the Medal column


setwd("/home/pruthvi/Desktop/Sem5/Data Analytics/Assignments/1st/120-years-of-olympic-history-athletes-and-results")
#dataset <- read.table("athlete_events.csv", sep=",")
#print(dataset)
library(dplyr)
dataset <- read.csv("athlete_events.csv",TRUE,",")

#Q1

indians <- subset(dataset,dataset$Team == "India",select = c(Name,Event,Year))
indian_participants <- unique(indians[,1])
events <- unique(indians[,2])
years <- unique(indians[,3])
years_from_1960 <- years[years>=1960]

#indian_medalists <- unique(subset(dataset,dataset$Team == "India"& ((dataset$Medal=='Gold' | dataset$P=='Gold' | dataset$Q=="Gold") | (dataset$Medal=='Silver' | dataset$P=='Silver' | dataset$Q=="Silver") | (dataset$Medal=='Bronze' | dataset$P=='Bronze' | dataset$Q=="Bronze")),Name))
indian_medalists <- unique(subset(dataset,dataset$Team == "India"& ((dataset$Medal=='Gold') | (dataset$Medal=='Silver') | (dataset$Medal=='Bronze')),Name))
indian_medalists_names <- unlist(indian_medalists[,1],use.names = FALSE)

#unlist(my_set[,1], use.names=FALSE)

print("Indian medalists' Names : ")
print(indian_medalists_names)


print("Number of medals for each sport since 1960")
sports <- unique(subset(dataset,dataset$Team=="India" & (dataset$Medal=="Gold"|dataset$Medal=="Silver"|dataset$Medal=="Bronze"),select = c(Sport)))
winning_sports <- c()
no_of_medals <- c()
count <- 0
for(i in sports[,1]){
  k <- unique(subset(dataset,dataset$Team == "India"& ((dataset$Medal=='Gold') | (dataset$Medal=='Silver') | (dataset$Medal=='Bronze')) & dataset$Sport==i & dataset$Year >= 1960,Year))
  #k<- unique(subset(k,dataset$Year >=1960))
  if(length(k[,1])>0){
    count <- count+1
    print(i)
    winning_sports[count] <- i
    no_of_medals[count] <- length(k[,1])
    print(length(k[,1]))
  }
}

print("Total number of medals won by India in different sports since 1960")
no_of_medals <- length(unique(subset(dataset,dataset$Team=="India" & (dataset$Medal=="Gold"|dataset$Medal=="Silver"|dataset$Medal=="Bronze") & dataset$Year>= 1960,select = c(Sport,Year,Medal)))[,1])
print(no_of_medals)
k <- unique(subset(dataset,dataset$Team=="India" & (dataset$Medal=="Gold"|dataset$Medal=="Silver"|dataset$Medal=="Bronze") & dataset$Year>= 1960,select = c(Sport,Year,Medal)))

print("Sport with the highest number of medals : ")

sports <- unlist(k[,1], use.names=FALSE)
most_medal_game <- names(table(sports))[table(sports)==max(table(sports))]
print(most_medal_game)


