# irrigation
# Bashayr Alghamdi
# 10.10.2020
# A small case study for irrigation wide

# load packages
library(tidyverse)

#Begin with wide "messy" format:
irrigation <- read.csv("Data/irrigation_wide.csv")

#Examine the data
glimpse(irrigation)
summary(irrigation)

irrigation

#In 2007,what is the total area under irrigation
#for only the Americas
irrigation %>%
  filter(year == 2007) %>%
  select(ends_with("erica"))

irrigation %>%
  filter(year == 2007) %>%
  select(4, 5)%>%
  sum()

#to answer the following question we should use tidy data 
irrigation_t <- irrigation%>%
  pivot_longer(-year, names_to = "region")
irrigation_t

#what is the total area under irrigation in each year?
irrigation_t %>%
  group_by(year) %>%
  summarise(total = sum(value))

#standardize against 1980 (relative change over 1980)(easier)

irrigation_t%>%
  filter(year == 1980) %>% 
  mutate(rate=c(0,diff(value)/value[-length(value)]))

#Plot area over time for each region?

ggplot(irrigation_t, aes(x =region , y = value)) +
  geom_jitter(aes(color = year),
              alpha = 0.5,
              width = 0.05,
              height = 0.2,
              size=2)

#which regions increased the most from 1980 to 2007?
#arrange() reorders from lowest to highest ("ascending")
#and with - it's the opposite ("descending")
#slice() takes specific row numbers

irrigation_t%>%
  group_by(region)%>%
  summarise(diff = value[year==2007] - value[year ==1980])%>%
  arrange(-diff)%>%
  slice(1:2)

#our use top_n()
irrigation_t%>%
  group_by(region)%>%
  summarise(diff = value[year==2007] - value[year ==1980])%>%
  slice_max(diff, n = 2)

#what is the rate-of-change in each region?(maybe tricky)
irrigation_t %>% 
  arrange(region) %>% 
  group_by(region) %>% 
  mutate(rate=c(0,diff(value)/value[-length(value)]))->irrigation_rate
irrigation_rate

#where is it the lowest and highest?
irrigation_rate %>% 
  ungroup() %>% 
  slice_max(rate,n=1)

irrigation_rate %>% 
  ungroup() %>% 
  slice_min(rate,n=1)
