# Diamond Analysis
# Bashayr Alghamdi
# 01.10.2020
# A small case study for EDA and stats

# load packages
library(tidyverse)
library(Hmisc)
# Read in the data (csv format):
# Newer methods from tidyr package
jems <- read_csv("Data/diamonds.csv")

# super convenient way
# library(rio) # R i/o
# jems2 <- import("data/diamonds.csv")


#Exercise 8.2 (Examine structure) ----
# Get familiar with dataset
summary(jems)
names(jems)
glimpse(jems)
class(jems)

# Attributes
attributes(jems)
typeof(jems)

jems <- as_tibble(jems)
jems
# Exercise 8.3 (Counting individual groups) -----
# - How many diamonds with a clarity of category “IF” are present in the data-set? 

##########
ifcount <- sum(jems$clarity=="IF")
jems$clarity
ifcount##1790
#########

ifcount <- jems %>%
  filter(clarity=='IF') %>% 
  count()
IF_count <- ifcount$n
IF_count##1790

# - What fraction of the total do they represent?
############
tot_clarity <- length(jems$clarity)
tot_clarity##53940
ifcount/tot_clarity ##0.03318502
ifcount/tot_clarity*100 ##3.318502
###########

count_total <- jems %>%
  count()
Total_count <- count_total$n

fractionOfIF <- IF_count/Total_count

fractionOfIF##0.03318502

# Exercise 8.4 (Summarizing proportions) ----
# What proportion of the whole is made up of each category of clarity?
clarity <- jems%>%
  filter(clarity=="IF")
clarity

jems %>%
  group_by(clarity) %>%
  count()%>% ##count for each value of clarity as new column
  mutate(prop = n/tot_clarity) ##each count value divided by total



# Exercise 8.5 (Find specific diamonds prices)
# What is the cheapest diamond price overall? 
cheapestPrice <- jems %>%
  summarise(min(price))
cheapestPrice

# What is the range of diamond prices? 
rangePrice <- jems %>%
  summarise(range(price))
rangePrice

# What is the average diamond price in each category of cut and color?
jems %>%
  group_by(cut,color)%>%
  summarise(avg=mean(price))

# Exercise 8.6 (Basic plotting)----
# Make a scatter plot that shows the price of a diamond as described 
# by another continous variable, like the carat.
ggplot(jems,aes(carat,price)) +
  geom_point()
#Exercise 8.7 ----
#(Interpreting plots)
#**What can you say about the relationship between these two variables? 
##Do you think that you can use the carat weight of 
##a diamond to predict its price?


#Exercise 8.8 (Applying transformations) ----
#**Using the functions we discuss earlier, and in class, 
#apply a log10 transformation to both the price and carat.
#You can save these as new columns in the data set called`price_log10`and`
#carat_log10`.
carat_log10 <- log10(jems$carat)
carat_log10
price_log10 <- log10(jems$price)
price_log10
jems2 <- data.frame(jems,carat_log10,price_log10)
jems2

#Exercise 8.9 (Basic plotting) ----
#**Make a scatter plot that shows the price of a diamond as described by 
#*another continous variable, like the carat.
ggplot(jems2,aes(carat_log10,price_log10)) +
  geom_point()

#Exercise 8.10 (Viewing models) ----
#describes the relatioship shown in the plot?
jems_lm <- lm(price_log10 ~ carat_log10,data=jems2)
jems_lm

#Exercise 8.11 (Displaying models) ----
#Now that we’ve described the diamond price given a single variable, 
#can you display that on the plot?
ggplot(jems_lm,aes(carat_log10,price_log10)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Extra Ex ----  
jems %>%
  filter(clarity=='VVS2' & cut=='Good')

jems %>%
  filter(clarity == "VVS1", price < 1000) %>% 
  select(cut, carat, price)

jems %>%
  select(starts_with("c"))

jems %>%
  select(-starts_with("c"))

jems %>%
  select(price, carat)
  

jems %>%
  select(price, carat) %>%
  log10()

jems %>%
  select(price, carat) %>%
  log10() %>%
  bind_cols(jems)

