install.packages("tidyverse")
library(tidyverse)

#Loading data -----

d1 <- read_csv("2022 Apr.csv")
head(d1)

#installing dplyr for data manipulation -----
library(dplyr)
library(lubridate)

#Transformation of data into insights -----
transformed_data <- d1 %>%   select (rideable_type, started_at, ended_at, member_casual) %>% mutate (ride_length = difftime( ended_at, started_at, units ='mins'),  start_day = weekdays(started_at), ride_length_int = round(ride_length),   start_date = as.Date(started_at) )

#check if ride length is 0 or less than 0
transformed_data %>%  filter(ride_length_int <= 0) %>% count()

# 3493 zero minutes and 1 negative values , therefore removing these values.
clean_data <- transformed_data %>%   filter(ride_length_int > 0)count(clean_data)

#check for unique entities in rideable_type
unique (transformed_data$rideable_type)

# 3 types - classic, electric and docked
summary(clean_data)

#Now, we plot the data -----
library(ggplot2)

#1- First plot, we understand the difference in the members and casuals.
ggplot(data= clean_data) + geom_bar(mapping=aes(x = member_casual), fill="coral") + theme_classic()+ labs(title = "Retained customers", x= "Casuals and Members")

#2- Total usage of type of bikes for members/casuals.
ggplot(data= clean_data) + geom_bar(mapping=aes(x=rideable_type, fill = member_casual)) + labs(x="type of bike", y= "total usage for members and casuals")

#3- For order the days
clean_data$start_day <- factor(clean_data$start_day, c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday", "Sunday"))

#Weekly usage of bikes by members/casuals
ggplot(data= clean_data, mapping=aes(x=start_day, fill = member_casual)) + geom_bar(position= position_dodge()) + labs(x="weekdays",y="Users", title= "Weekly bike users", subtitle ="Difference in weekly bike usage - members and casuals")
ggplot(data= clean_data, mapping=aes(x=start_day, fill =rideable_type)) + geom_bar(position= position_dodge()) + labs(x="weekdays",y="Bikes used", title= "Weekly bikes users", subtitle ="Difference in weekly bike used by members and casuals")
