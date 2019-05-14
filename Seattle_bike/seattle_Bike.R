# install.packages("readr")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("scales")
# install.packages("magrittr") # only needed the first time you use it
# install.packages("dplyr")    # alternative installation of the %>%
# install.packages("ggplot2")
#install.packages("knitr")
library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)
library(ggplot2)
library(knitr)

library(readr)
library(tidyverse)
library(lubridate)
library(scales)

bike_traffic_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")
# bike_traffic %>%
#   count(crossing, direction)

bike_traffic <- bike_traffic_raw %>%
  mutate(date = mdy_hms(date))%>%
  filter(bike_count < 2000)%>%
  select(-ped_count)



bike_traffic %>%
  ggplot(aes(date, fill = is.na(bike_count))) +
  geom_histogram() +
  facet_grid(crossing ~ direction)+
  labs(title = "Missing data 'Year and direction-wise bike count at each crossing' ")

bike_traffic %>%
    count(crossing = "Broadway Cycle Track North Of E Union St", direction = "North", ped_count = NA)

bike_traffic %>%
  group_by(crossing, hour = hour(date)) %>%
  summarize(bike_count = sum(bike_count, na.rm = TRUE)) %>%
  mutate(pct_bike = bike_count/sum(bike_count))%>%
  ggplot(aes(hour, pct_bike, color =crossing)) +
  geom_line() +
  geom_point()+
  scale_y_continuous(labels = percent_format())+
  labs(title = "Percentage of bikes on a crossing per hour")
# Busiest time for seattle bikers is 9 am and 5 pm (working hours) and most busiest crossing is Elliot Bay Trail
  
bike_by_time_window <- bike_traffic %>%
  mutate(hour  = hour(date)) %>%
  mutate(time_window = case_when(
    between(hour, 7, 10) ~ "Morning Commute",
    between(hour, 11, 15) ~ "Midday",
    between(hour, 16, 18) ~ "Night Commute",
    TRUE ~ "Night"
  ))%>% 
  group_by(crossing,time_window) %>%
  summarise(number_missing = sum(is.na(bike_count)),
            bike_count = sum(bike_count, na.rm = TRUE))%>%
  mutate(pct_bike = bike_count/sum(bike_count))

bike_by_time_window %>% 
  select( -number_missing, -bike_count)%>%
  spread(time_window, pct_bike)

bike_traffic %>%
  group_by(crossing, weekday = wday(date, label = TRUE),
                           hour = hour(date)) %>%
  summarize(total_bikes = sum(bike_count, na.rm =TRUE)) %>%
  group_by(crossing)%>%
  mutate(pct_bike = total_bikes/sum(total_bikes))%>%
  
  ggplot(aes(hour, pct_bike, color = crossing))+
  geom_line()+
  facet_grid(crossing ~ weekday)+
  labs(title = "Percentage of bikes on a crossing on a day of the week per hour")



bike_traffic %>%
  group_by(crossing, month = fct_relevel(month.name[month(date)], month.name)) %>%
  summarize(total_bikes = sum(bike_count), na.rm =TRUE)%>%
  mutate(pct_bike =total_bikes/sum(total_bikes))%>%
  ggplot(aes(month, pct_bike, color = crossing, group =crossing))+
  geom_line()+
  expand_limits(y = 0)+
  scale_y_continuous(labels = percent_format())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Percentage of bikes on a crossing month-wise")



##### What directions do people comute by bike?

bike_by_time_window_hour_crossing <- bike_traffic %>%
  filter(crossing != 'MTS Trail',
           !wday(date, label =TRUE)%in% c("Sat","Sun"))%>%
  mutate(hour  = hour(date)) %>%
  group_by(crossing,direction,hour) %>%
  summarise(bike_count = sum(bike_count, na.rm = TRUE))%>%
  mutate(pct_bike = bike_count/sum(bike_count)) 

bike_by_time_window_hour_crossing %>%
  group_by(crossing)%>%
  mutate(average_hour = sum(hour * pct_bike)[direction = 'North'])%>%
  ungroup()%>%
  mutate(crossing = fct_reorder(crossing,average_hour))%>%
  ggplot(aes(hour, pct_bike, color =direction))+
  geom_line()+
  facet_grid(crossing ~ .)+
  scale_y_continuous(labels =percent_format())+
  labs(x = "Time of week", title ="Percentage of bikes in time of week in different directions")
