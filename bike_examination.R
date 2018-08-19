library(tidyverse)
library(lubridate)

#wmata open data -- where they used to come from and where they don't come from anymore. 


#look and see what the differencecs 

#look and see what did the most trips

#goup yb hour and see what that aily houely usage is like --look at when usage peaks 

bikes <- read_csv("bike_share_data.csv")

glimpse(bikes) 

#let's just look at the bike companies and their unique bikes. And then see how many trips each bike took. 
bike_companies <- bikes  %>%
    group_by(`Operator`, `BikeID`) %>% 
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n()) %>%
  #arrange the list in descending order
  arrange(desc(count)) 

#now lets see how many bikes each company had

unique_bikes <- bike_companies %>%
  group_by(`Operator`)%>% 
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n()) %>%
  #arrange the list in descending order
  arrange(desc(count)) 
  

#Sorts the bike column in a format that will hopefully help me calculate the time elapsed between first and last time bike was checked out

sorted_bikes<-bikes  %>% 
  arrange(desc(StartDate)) %>%
  arrange(desc(BikeID)) 


#THEN sets the start and end date as POSIXlt in a way that the computer gods smile upon...Was previously a character class
sorted_bikes$StartDate <- as.POSIXct(sorted_bikes$StartDate, format = "%m/%d/%y %H:%M")
sorted_bikes$EndDate <- as.POSIXct(sorted_bikes$EndDate, format = "%m/%d/%y %H:%M")






#this was a t3est to look at the feasibility of total days between the first time it was ridden and the last time it was ridden.
test <- sorted_bikes %>%
  filter(BikeID =="ZZSN7KWMSYXI7")
difftime(max(test$EndDate), min(test$StartDate), tz= EST, units = "days")

#now I'm trying to 

new_df <- sorted_bikes %>%
  group_by(`Operator`, BikeID) %>%
  summarize(Earliest=min(StartDate), Latest=max(EndDate), days=(difftime(Latest, Earliest, tz= EST, units = "days")))

average_bike_life <- new_df %>%
  group_by(Operator) %>%
  summarize(Mean = mean(days, na.rm=TRUE))

