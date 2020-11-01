#title: "Hotels"
#author: "Bashayr Alghamdi"
#date: "10/10/2020"
#analysis about hotels booking

# load packages
library(tidyverse)
library(knitr)
library(dplyr)

# Get the Data
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')
hotels

# Get familiar with dataset
summary(hotels)
glimpse(hotels)
names(hotels)
class(hotels)

#remove NA value
hotels_new <- hotels%>% 
  na.omit() 

#tidy data 
#arrival date
#gather the date of arrival in one column
hotels %>%
  unite(arrival_data,
        arrival_date_day_of_month,
        arrival_date_month,
        arrival_date_year,
        sep="-")->hotels_date 

hotels_date

library(lubridate)

glimpse(hotels_date)
hotels <- hotels %>% 
  mutate(arrival_date = dmy(paste(
    arrival_date_day_of_month,
    arrival_date_month,
    arrival_date_year)))
hotels$arrival_date[10]

#calculate total of the night that the customer stayed  
hotels %>% 
  select(stays_in_weekend_nights,stays_in_week_nights) %>% 
  mutate(stays_nights= stays_in_weekend_nights + stays_in_week_nights)->hotels_nights
 
hotels_nights[23,]

#Percentage of which month is there more reservation
hotels %>% 
  group_by(arrival_date_month) %>% 
  summarise(count=n(),
            Percentage=n()/nrow(hotels))->months_count


#Pie chart of parentage of all months
months_count
pie <- ggplot(data=months_count,aes(x="",y=Percentage,fill=arrival_date_month)) +
  geom_col(color= "white")+
  coord_polar("y",start = 0)+
  geom_text(aes(label=paste0(round(Percentage*100),"%")),
            position = position_stack(vjust=0.5))
pie  

hotels %>% 
  mutate(kids = children+babies) %>% 
  select(hotel,
         is_canceled,
         arrival_date_month,
         adults,
         kids,
         required_car_parking_spaces)->hotel_ds
hotel_ds$kids
  
#How mach family they are got reservation
hotels %>% 
  select(reservation_status,adults, children,babies) %>% 
  filter(reservation_status == "Check-Out") %>% 
  mutate(family=case_when(children+babies>0~ "yes",TRUE ~ "No"))->hotels_family

hotels_family %>% 
  group_by(family) %>% 
  summarise(count=n())


#How many customers take car parking space 


hotels %>% 
  select(adults, babies, children,required_car_parking_spaces) %>% 
  filter(babies+children == 0 & required_car_parking_spaces >= 1) %>% 
  summarise(count=n())#6,314 

hotels %>% 
  select(adults, babies, children,required_car_parking_spaces) %>% 
  filter(babies+children == 0 & required_car_parking_spaces >= 1) %>% 
  mutate(number_of_customer=adults+babies+children) %>% 
  ggplot(aes(x = number_of_customer,y = required_car_parking_spaces))+
  geom_jitter(alpha = 0.25)+
  scale_y_continuous(limits = c(0,5),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(0,5),
                     expand = c(0,0))

#How many customers that had kids take car parking space 
hotels %>% 
  select(adults, babies, children,required_car_parking_spaces) %>% 
  filter(babies+children != 0 & required_car_parking_spaces >= 1) %>% 
  summarise(count=n())#1,102

hotels %>% 
  select(adults, babies, children,required_car_parking_spaces) %>% 
  filter(babies+children != 0 & required_car_parking_spaces >= 1) %>% 
  mutate(number_of_customer=adults+babies+children) %>% 
  ggplot(aes(x = number_of_customer,y = required_car_parking_spaces))+
  geom_jitter(alpha = 0.25)+
  scale_y_continuous(limits = c(0,3),
                   expand = c(0,0)) +
  scale_x_continuous(limits = c(1,6),
                   expand = c(0,0)) 
  
  

#what is type of customer that take car parking space 
hotels %>% 
  select(customer_type,required_car_parking_spaces) %>% 
  filter(required_car_parking_spaces >= 1)#7,416

hotels %>% 
  select(adults, babies, children,required_car_parking_spaces) %>% 
  filter(babies+children != 0 & required_car_parking_spaces >= 1) %>% 
  summarise(count=n())#1,102

hotels %>% 
  select(adults, babies, children,required_car_parking_spaces) %>% 
  filter(babies+children != 0 & required_car_parking_spaces >= 1) %>% 
  mutate(number_of_customer=adults+babies+children) %>% 
  ggplot(aes(x = number_of_customer,y = required_car_parking_spaces))+
  geom_jitter(alpha = 0.25)+
  scale_y_continuous(limits = c(0,3),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(1,6),
                     expand = c(0,0))


#How many customers take car parking space 

hotels %>% 
  select(adults, babies, children,required_car_parking_spaces) %>% 
  filter(babies+children == 0 & required_car_parking_spaces >= 1) %>% 
  summarise(count=n())#6,314 

hotels %>% 
  select(adults, babies, children,required_car_parking_spaces) %>% 
  filter(babies+children == 0 & required_car_parking_spaces >= 1) %>% 
  mutate(number_of_customer=adults+babies+children) %>% 
  ggplot(aes(x = number_of_customer,y = required_car_parking_spaces))+
  geom_jitter(alpha = 0.25)+
  scale_y_continuous(limits = c(0,10),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(0,5),
                     expand = c(0,0))


