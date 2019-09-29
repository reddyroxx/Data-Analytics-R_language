setwd("/home/pruthvi/Desktop/Sem5/Data Analytics/Assignments/1st/120-years-of-olympic-history-athletes-and-results")


library(dplyr)
dataset <- read.csv("athlete_events.csv",TRUE,",")

years <- unique(subset(dataset,select = c(Year)))

country <- unique(subset(dataset,select = c(NOC)))


grp_by_year_country <- group_by(dataset,Year,NOC)

arrange(grp_by_year_country, Year,NOC)

my_set <- select(grp_by_year_country,NOC,Year,Sex)

male_ii <- subset(my_set,my_set$Sex=="M")
female_ii <- subset(my_set,my_set$Sex=="F")

counts_of_male <- count(male_ii,Sex)
counts_of_female <- count(female_ii,Sex)

#df <- data.frame(count(male_ii,Sex=="M")[,1],count(male_ii,Sex=="M")[,2],Male = count(male_ii,Sex)[,4],Female = count(female_ii,Sex)[,4])

new_male <- unique(subset(dataset,dataset$Sex=="M",select = c(Name,NOC)))
new_female <- unique(subset(dataset,dataset$Sex=="F",select = c(Name,NOC)))
male_count_by_country <- count(new_male,NOC)

female_count_by_country <- count(new_female,NOC)
