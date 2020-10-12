#title: "Hotels"
#author: "Bashayr Alghamdi"
#date: "10/10/2020"
#analysis about hotels booking

# load packages
library(tidyverse)
library(knitr)

# Get the Data
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')
hotels
# Get familiar with dataset
summary(hotels)
glimpse(hotels)
names(hotels)
class(hotels)


#tidy data arrival date
#gather the date of arrival in one column
hotels %>%
  unite(arrival_data,
        arrival_date_day_of_month,
        arrival_date_month,
        arrival_date_year,
        sep="-")->hotels_date 

hotels_date
glimpse(hotels_date)

#calculate total of the night that the customer stayed  
hotels %>% 
  select(stays_in_weekend_nights,stays_in_week_nights) %>% 
  mutate(stays_nights= stays_in_weekend_nights + stays_in_week_nights)->hotels_nights


#parentage of which month is 
total_count <- sum(months_count$n)

hotels %>% 
  count(arrival_date_month)-> months_count

months_count %>% 
  mutate(parentage= (n/total_count)*100)->months_count

months_count %>% 
  pie(parentage,labels =arrival_date_month)#'x' values must be positive.

summary(months_count)



