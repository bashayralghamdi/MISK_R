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


#parentage of which month is there more reservation

hotels %>% 
  group_by(arrival_date_month) %>% 
  summarise(count=n(),
            percantage=n()/nrow(hotels))->months_count
#pie chart of parentage of all months
#reference : https://www.youtube.com/watch?v=bzRD_nvvIVI 
months_count
pie <- ggplot(data=months_count,aes(x="",y=percantage,fill=arrival_date_month)) +
  geom_col(color= "white")+
  coord_polar("y",start = 0)+
  geom_text(aes(label=paste0(round(percantage*100),"%")),
            position = position_stack(vjust=0.5))
pie  


#how mach family they are got reservation
hotels %>% 
  select(reservation_status,adults, children,babies) %>% 
  filter(reservation_status == "Check-Out") %>% 
  mutate(family=case_when(children+babies>0~ "yes",TRUE ~ "No"))->hotels_family

hotels_family %>% 
  group_by(family) %>% 
  summarise(count=n())

