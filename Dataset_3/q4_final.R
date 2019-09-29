setwd("/home/pruthvi/Desktop/Sem5/Data Analytics/Assignments/3rd")

dataset <- read.csv("NewYearResolution15.csv",TRUE,",")

library(dplyr)
library(plotly)

category <- array(unique(dataset$Resolution_Category))

#Gender wise comparison
male_cat_counts <- c()
female_cat_counts <- c()

for(i in 1:length(category)){
  male_cat_counts[i] <- dim(subset(dataset,dataset$gender=="male" & dataset$Resolution_Category==category[i]))[1]
  female_cat_counts[i] <- dim(subset(dataset,dataset$gender=="female" & dataset$Resolution_Category==category[i]))[1]
}


m_bar <- plot_ly(x = category,y = male_cat_counts,title = "Male Resolutions",type = "bar")%>%
             layout(title = "Male Resolutions" , xaxis=list(title = "Resolution Category"),
                    yaxis = list(title = "Counts"))
print(m_bar)

f_bar <- plot_ly(x = category,y = female_cat_counts,title = "Female Resolutions",type = "bar")%>%
  layout(title = "Female Resolutions" , xaxis=list(title = "Resolution Category"),
         yaxis = list(title = "Counts"))
print(f_bar)

m_pie <- plot_ly(labels = category,values = male_cat_counts,title = "Male Resolutions",type = "pie",textposition = 'outside',
                 textinfo = 'label+percent')%>%
  layout(title = "Male Resolutions")
print(m_pie)

f_pie <- plot_ly(labels = category,values = female_cat_counts,title = "Female Resolutions",type = "pie",textposition = 'outside',
                 textinfo = 'label+percent')%>%
  layout(title = "Female Resolutions")
print(f_pie)



#Region wise comparison
regions <- array(unique(dataset$tweet_region))

west_counts <- c()
midwest_counts <- c()
south_counts <- c()
northeast_counts <- c()




for(i in 1:length(category)){
  west_counts[i] <- dim(subset(dataset,dataset$tweet_region=="West" & dataset$Resolution_Category==category[i]))[1]
  midwest_counts[i] <- dim(subset(dataset,dataset$tweet_region=="Midwest" & dataset$Resolution_Category==category[i]))[1]
  south_counts[i] <- dim(subset(dataset,dataset$tweet_region=="South" & dataset$Resolution_Category==category[i]))[1]
  northeast_counts[i] <- dim(subset(dataset,dataset$tweet_region=="Northeast" & dataset$Resolution_Category==category[i]))[1]
}

w_pie <- plot_ly(labels = category,values = west_counts,type = "pie",textposition = 'outside',
                 textinfo = 'label+percent')%>%
  layout(title = "Western People Resolutions")
print(w_pie)

mw_pie <- plot_ly(labels = category,values = midwest_counts,type = "pie",textposition = 'outside',
                 textinfo = 'label+percent')%>%
  layout(title = "Mid-Western People Resolutions")
print(mw_pie)

s_pie <- plot_ly(labels = category,values = south_counts,type = "pie",textposition = 'outside',
                 textinfo = 'label+percent')%>%
  layout(title = "Southern People Resolutions")
print(s_pie)

ne_pie <- plot_ly(labels = category,values = northeast_counts,type = "pie",textposition = 'outside',
                 textinfo = 'label+percent')%>%
  layout(title = "NorthEastern People Resolutions")
print(ne_pie)



#State wise analysis

tw_states <- array(unique(dataset$tweet_state))

state_counts <- c()

for(i in 1:length(tw_states)){
  state_counts[i] <- dim(subset(dataset,dataset$tweet_state==tw_states[i]))[1]
}


tw_per_state <- plot_ly(type = 'choropleth',
             locations = tw_states,locationmode = "USA-states",
             colorscale = "Viridis",
             z = state_counts
             ) %>%
  colorbar(title = "Number of Tweets") %>%
  layout(geo = list(scope= "usa") , title = "Tweets per State")
print(tw_per_state)



#Conclusions from tw_per_state : It can be seen that most of the tweets were from
# CA , NY , TX , FL and IL (top 5) ... this speaks of the states' usage of 
# social media or computer devices (this could mean that other states have less 
# population and/or limited access to computers or social media)
