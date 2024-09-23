# Load R packages
library(tidyverse)
library(conflicted)
library(hms)
library(dplyr) 
library(lubridate)
library(ggplot2)
library(skimr)
library(janitor)
library(data.table)

# Load .csv files for July 2023 - July 2024
jul23 <- read_csv("C:/Users/Andrew/OneDrive/Documents/_PERSONAL/Education/Tech/Data-Analytics/Google_Data-Analytics_Coursera/8_CapstoneProject/Case-Study-1_Cyclistic/Cyclistic-Datasets_07.2023-07.2024/202307-divvy-tripdata.csv")
aug23 <- read_csv("C:/Users/Andrew/OneDrive/Documents/_PERSONAL/Education/Tech/Data-Analytics/Google_Data-Analytics_Coursera/8_CapstoneProject/Case-Study-1_Cyclistic/Cyclistic-Datasets_07.2023-07.2024/202308-divvy-tripdata.csv")
sep23 <- read_csv("C:/Users/Andrew/OneDrive/Documents/_PERSONAL/Education/Tech/Data-Analytics/Google_Data-Analytics_Coursera/8_CapstoneProject/Case-Study-1_Cyclistic/Cyclistic-Datasets_07.2023-07.2024/202309-divvy-tripdata.csv")
oct23 <- read_csv("C:/Users/Andrew/OneDrive/Documents/_PERSONAL/Education/Tech/Data-Analytics/Google_Data-Analytics_Coursera/8_CapstoneProject/Case-Study-1_Cyclistic/Cyclistic-Datasets_07.2023-07.2024/202310-divvy-tripdata.csv")
nov23 <- read_csv("C:/Users/Andrew/OneDrive/Documents/_PERSONAL/Education/Tech/Data-Analytics/Google_Data-Analytics_Coursera/8_CapstoneProject/Case-Study-1_Cyclistic/Cyclistic-Datasets_07.2023-07.2024/202311-divvy-tripdata.csv")
dec23 <- read_csv("C:/Users/Andrew/OneDrive/Documents/_PERSONAL/Education/Tech/Data-Analytics/Google_Data-Analytics_Coursera/8_CapstoneProject/Case-Study-1_Cyclistic/Cyclistic-Datasets_07.2023-07.2024/202312-divvy-tripdata.csv")
jan24 <- read_csv("C:/Users/Andrew/OneDrive/Documents/_PERSONAL/Education/Tech/Data-Analytics/Google_Data-Analytics_Coursera/8_CapstoneProject/Case-Study-1_Cyclistic/Cyclistic-Datasets_07.2023-07.2024/202401-divvy-tripdata.csv")
feb24 <- read_csv("C:/Users/Andrew/OneDrive/Documents/_PERSONAL/Education/Tech/Data-Analytics/Google_Data-Analytics_Coursera/8_CapstoneProject/Case-Study-1_Cyclistic/Cyclistic-Datasets_07.2023-07.2024/202402-divvy-tripdata.csv")
mar24 <- read_csv("C:/Users/Andrew/OneDrive/Documents/_PERSONAL/Education/Tech/Data-Analytics/Google_Data-Analytics_Coursera/8_CapstoneProject/Case-Study-1_Cyclistic/Cyclistic-Datasets_07.2023-07.2024/202403-divvy-tripdata.csv")
apr24 <- read_csv("C:/Users/Andrew/OneDrive/Documents/_PERSONAL/Education/Tech/Data-Analytics/Google_Data-Analytics_Coursera/8_CapstoneProject/Case-Study-1_Cyclistic/Cyclistic-Datasets_07.2023-07.2024/202404-divvy-tripdata.csv")
may24 <- read_csv("C:/Users/Andrew/OneDrive/Documents/_PERSONAL/Education/Tech/Data-Analytics/Google_Data-Analytics_Coursera/8_CapstoneProject/Case-Study-1_Cyclistic/Cyclistic-Datasets_07.2023-07.2024/202405-divvy-tripdata.csv")
jun24 <- read_csv("C:/Users/Andrew/OneDrive/Documents/_PERSONAL/Education/Tech/Data-Analytics/Google_Data-Analytics_Coursera/8_CapstoneProject/Case-Study-1_Cyclistic/Cyclistic-Datasets_07.2023-07.2024/202406-divvy-tripdata.csv")
jul24 <- read_csv("C:/Users/Andrew/OneDrive/Documents/_PERSONAL/Education/Tech/Data-Analytics/Google_Data-Analytics_Coursera/8_CapstoneProject/Case-Study-1_Cyclistic/Cyclistic-Datasets_07.2023-07.2024/202407-divvy-tripdata.csv")

# Merge the CLEANED datasets into one 12-month overview dataframe (cyclistic_merged)
cyclistic_df <- rbind(jul23,aug23,sep23,oct23,nov23,dec23,jan24,feb24,mar24,apr24,may24,jun24,jul24)

# Remove irrelevant columns
cyclistic_df <- cyclistic_df %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, start_station_id, end_station_id, end_station_name))

# Calculate Total Rides

## TOTAL RIDES
### total rides
nrow(cyclistic_date)

## MEMBER TYPE
### total rides by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  count(member_casual)

## BIKE TYPE
### total rides for each bike type
cyclistic_date %>% 
  group_by(rideable_type) %>% 
  count(rideable_type)

### total rides by member type and bike type
cyclistic_date %>% 
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

## HOUR
### total rides for each hour of the day
cyclistic_date %>% 
  count(hour) %>% 
  print(n = 24)

### total rides for each hour of the day by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  count(hour) %>% 
  print(n = 48)

## TIME OF DAY
### total rides in the morning
cyclistic_date %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

### total rides in the morning by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

### total rides in the afternoon
cyclistic_date %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

### total rides in the afternoon by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

### total rides in the evening
cyclistic_date %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

### total rides in the evening by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

### total rides at night
cyclistic_date %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

### total rides at night by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

### total rides at all times of the day
cyclistic_date %>%
  group_by(time_of_day) %>% 
  count(time_of_day)

### total rides at all times of the day by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(time_of_day)

## DAY OF THE WEEK
### total rides by day of the week
cyclistic_date %>%
  count(day_of_week)

### total rides by day of the week and member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  count(day_of_week)

## DAY OF THE MONTH
### total rides by day of the month
cyclistic_date %>% 
  count(day) %>% 
  print(n = 31)

### total rides by day of the month and member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  count(day) %>% 
  print(n = 62)

## MONTH OF THE YEAR
### total rides by month
cyclistic_date %>% 
  count(month)

### total rides by month and by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 24)

## SEASON
## SPRING
### total rides in Spring
cyclistic_date %>% 
  filter(season == "Spring") %>% 
  count(season)

### total rides in Spring by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  count(season)

## SUMMER
### total rides in Summer
cyclistic_date %>% 
  filter(season == "Summer") %>% 
  count(season)

### total rides in Summer by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  count(season)

## FALL
### total rides in Fall
cyclistic_date %>% 
  filter(season == "Fall") %>% 
  count(season)

### total rides in Fall by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  count(season)

## WINTER
### total rides in Winter
cyclistic_date %>% 
  filter(season == "Winter") %>% 
  count(season)

### total rides in Winter by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  count(season)

## ALL SEASONS
### total rides by each season
cyclistic_date %>% 
  group_by(season) %>% 
  count(season)

### total rides by each season by member type
cyclistic_date %>% 
  group_by(season, member_casual) %>% 
  count(season)

## AVERAGE RIDE LENGTH
### average ride_length
cyclistic_avgRide <- mean(cyclistic_date$ride_length)
print(cyclistic_avgRide)

### average ride_length by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride_length by type of bike
cyclistic_date %>% 
  group_by(rideable_type) %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride_length by type of bike and member type
cyclistic_date %>% 
  group_by(member_casual, rideable_type) %>%
  summarise_at(vars(ride_length), list(time = mean))

## AVERAGE RIDE LENGTH BY HOUR
### average ride_length by the hour
cyclistic_date %>% 
  group_by(hour) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=24)

### average ride_length by the hour for each member type
cyclistic_date %>% 
  group_by(hour, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=48)

## AVERAGE RIDE LENGTH BY TIME OF DAY
### average ride length in the morning
cyclistic_date %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length in the morning for each member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length in the afternoon
cyclistic_date %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length in the afternoon for each member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length in the evening
cyclistic_date %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length in the evening for each member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length at night
cyclistic_date %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length at night for each member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length), list (time = mean))

### average ride length during all times of day
cyclistic_date %>% 
  group_by(time_of_day) %>% 
 summarise_at(vars(ride_length), list(time = mean))

### average ride length during all times of day for each member type
cyclistic_date %>% 
  group_by(time_of_day, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean))

## DAY OF THE WEEK
### average ride length for each day of the week
cyclistic_date %>% 
  group_by(day_of_week) %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length for each day of the week for each member type
cyclistic_date %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length), list(time = mean))

## DAY OF THE MONTH
### average ride length for each day of the month
cyclistic_date %>% 
  group_by(day) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=31)

### average ride length for each day of the month for each member type
cyclistic_date %>% group_by(day, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=62)

## MONTH OF THE YEAR
### average ride length for each month
cyclistic_date %>% 
  group_by(month) %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length for each month by member type
cyclistic_date %>% 
  group_by(month, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=24)

## SEASONS
### average ride length in Spring
cyclistic_date %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length in Spring for each member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length in Summer
cyclistic_date %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length in Summer for each member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length in Fall
cyclistic_date %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length in Fall for each member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length in Winter
cyclistic_date %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length in Winter for each member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length for all seasons
cyclistic_date %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length), list(time = mean))

### average ride length for all seasons for each member type
cyclistic_date %>% 
  group_by(season, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean))

# SHARE

## VISUAL 1: TOTAL RIDES BY CUSTOMER TYPE (Members 63.66% vs Casual 36.34%)
View(cyclistic_date)
cyclistic_date %>%
  group_by(member_casual)%>%
  summarize(number_of_rides = n())%>%
  arrange(member_casual)%>%
  ggplot(aes(x = member_casual, y = number_of_rides, fill = member_casual)) +
  labs(title = "Total Rides By Customer Type") +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

## VISUAL 2: AVERAGE RIDE LENGTH BY CUSTOMER TYPE (CASUAL AVG 30.35 vs. MEMBER AVG 13.42)
cyclistic_date %>%
  group_by(member_casual)%>%
  summarize(average_ride_length = mean(ride_length))%>%
  ggplot(aes(x = member_casual, y = average_ride_length, fill = member_casual)) +
  labs(title = "Average Ride Length by Customer Type") +
  geom_col(width = 0.5, position = position_dodge(width = 0.5))

## VISUAL 3: BUSIEST TIMES BY CUSTOMER TYPE (BOTH TYPES PEAK AT 5PM)
cyclistic_date %>%
  group_by(member_casual, hour) %>%
  summarize(number_of_trips = n()) %>%
  ggplot(aes(x = hour, y = number_of_trips, color = member_casual, group = member_casual)) +
  geom_line() +
  labs(title = "Bike Demand by Hour", x = "Time of Day") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


## VISUAL 4: BUSIEST WEEKDAY BY CUSTOMER TYPE
cyclistic_date %>%
  group_by(member_casual, day_of_week) %>%
  summarize(number_of_rides = n()) %>%
  mutate(day_of_week = factor(day_of_week, 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title = "Total Rides by Weekday") +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## VISUAL 5: AVERAGE RIDE LENGTH BY WEEKDAY
cyclistic_date %>%
  group_by(member_casual, day_of_week) %>%
  summarize(average_ride_length = mean(ride_length)) %>%
  mutate(day_of_week = factor(day_of_week, 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) +
  labs(title = "Average Ride Length by Weekday") +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
## VISUAL 6: BUSIEST MONTH BY CUSTOMER TYPE
cyclistic_date %>%
  group_by(member_casual, month)%>%
  summarize(number_of_rides = n())%>%
  arrange(member_casual, month)%>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title = "Total Rides by Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

## VISUAL 7: MOST POPULAR BIKE BY CUSTOMER TYPE
cyclistic_date %>%
  group_by(rideable_type, member_casual)%>%
  summarize(number_of_trips = n())%>%
  ggplot(aes(x = rideable_type, y = number_of_trips, fill = member_casual)) +
  geom_bar(stat = 'identity') +
  labs(title = "Total Rides by Bike Type") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

## KEY FINDINGS:

### Bike Rides and Average Ride Length

-Members took the most bike rides (63.66%) of the total trips, compared to 36.34% of total trips by Casual riders\
-However, Casual riders took longer bike rides (Average Ride Length \~31 minutes) than Members (Average Ride Length \~13 minutes)\

### Bike Demand

-The busiest time of the day for both Members and Casual riders peaked in the Afternoon at around 17:00 hr (5 pm)\
-Member riders' activity also spiked between 07:00 and 08:00 hr\
-The top 3 busiest weekdays for Casual riders were Saturday, Sunday, and Friday\
-The top 3 busiest weekdays for Member riders were Wednesday, Tuesday, and Thursday\
-The busiest season for both customer types was the Summer\
-Casual rides peak in the Summer months and drops dramatically (-57%) in the Fall\
-Member rides remained relatively high through the Fall and in Winter\

### Bike Type Popularity

-The most popular bike for both customer types was the Classic Bike\
-Of all rides, 59.59% were done on the Classic Bike\
-The second most popular bike was the Electric Bike (39.77% of all rides)\
-Among Electric Bike users, Member riders (1,342,789; 62.47%) used it more than Casual riders (806,567; 37.53%)\
-There were no Member rides on the Docked Bikes\
-Only Casual riders used the Docked Bikes, which only accounted for 34,286 rides (0.63% of all rides) across the 12 months\


# PHASE 6: ACT

### MY TOP THREE RECOMMENDATIONS:

1.  **Introduce Seasonal and Weekend Membership Plans**:\
    -**Rationale**: Casual riders peak during weekends (Friday to Sunday) and the Summer months, indicating Casual riders' preference for flexible and recreational usage.\
    -**Implementation**: Offer a "Weekend Warrior" membership that allows unlimited rides on weekends at a lower cost, and a "Summer Membership" that allows unlimited rides or discounts during peak Summer months. These additional membership options can act as "gateway memberships," allowing casual riders to experience the benefits of membership without a year-long commitment.\

2.  **Offer Ride Length Benefits for Members**:\
    -**Rationale**: Casual riders tend to take longer rides (average ~31 minutes) than members (average ~13 minutes). Highlighting the benefits of a membership that includes longer ride times without additional fees can attract casual riders.\
    -**Implementation**: Adjust membership benefits to include longer ride durations (e.g., 45-60 minutes) before additional charges apply, specifically catering to casual riders who enjoy extended rides.\

3.  **Launch Incentive Programs for Casual to Member Conversion**:\
-**Rationale**: Providing incentives can create a low-risk opportunity for casual riders to explore membership benefits.\
-**Implementation**: Introduce a "Try Before You Commit" program that offers a one-month trial membership after their first few rides or a discount on membership after reaching a set number of rides. Use digital channels to promote these offers directly to casual riders, emphasizing the cost-saving benefits of becoming a member.\
    
### OTHER RECOMMENDATIONS TO CONSIDER:

4.  **Create and Promote Commuter-Friendly Membership Packages**:\
    -**Rationale**: Member riders show a pattern of using bikes for commuting, peaking during weekday mornings and afternoons. Positioning bikes as a commuting solution can attract casual riders who may consider membership for convenience.\
    -**Implementation**: Develop and promote a "Commuter Membership" with perks that include bike availability during rush hours, quick access to bike stations, and/or longer rental time. Digital campaigns targeting city commuters can highlight the time and cost of savings using Cyclistic for daily commutes.\

5.  **Boost the Popularity of Electric Bikes with Targeted Campaigns**:\
-**Rationale**: Electric Bikes are used less frequently despite offering an easier and faster riding experience. Increasing their usage can attract casual riders who might find Classic Bikes too physically demanding.\
-**Implementation**: Offer Electric Bike-focused promotions such as "Electric Bike Experience Days" where casual riders can try them at a discounted rate. Additionally, create exclusive membership tiers featuring enhanced Electric Bike access, making it a compelling reason for casual riders to upgrade to membership.\