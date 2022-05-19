install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")
install.packages("lubridate")


library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)

# read csv files 
df1 <-read_csv("202101-divvy-tripdata.csv")
df2 <-read_csv("202102-divvy-tripdata.csv")
df3 <-read_csv("202103-divvy-tripdata.csv")
df4 <-read_csv("202104-divvy-tripdata.csv")
df5 <-read_csv("202105-divvy-tripdata.csv")
df6 <-read_csv("202106-divvy-tripdata.csv")
df7 <-read_csv("202107-divvy-tripdata.csv")
df8 <-read_csv("202108-divvy-tripdata.csv")
df9 <-read_csv("202109-divvy-tripdata.csv")
df10 <-read_csv("202110-divvy-tripdata.csv")
df11 <-read_csv("202111-divvy-tripdata.csv")
df12 <-read_csv("202112-divvy-tripdata.csv")

View(df1)


df1 %>%
  mutate(ride_length = as.duration(ride_length))
df2 %>%
  mutate(ride_length = as.duration(ride_length))
df3 %>%
  mutate(ride_length = as.duration(ride_length))
df4 %>%
  mutate(ride_length = as.duration(ride_length))
df5 %>%
  mutate(ride_length = as.duration(ride_length))
df6 %>%
  mutate(ride_length = as.duration(ride_length))
df7 %>%
  mutate(ride_length = as.duration(ride_length))
df8 %>%
  mutate(ride_length = as.duration(ride_length))
df9 %>%
  mutate(ride_length = as.duration(ride_length))
df10 %>%
  mutate(ride_length = as.duration(ride_length))
df11 %>%
  mutate(ride_length = as.duration(ride_length))
df12 %>%
  mutate(ride_length = as.duration(ride_length))

#preview data types of each of the data frames        
str(df1)
str(df2)
str(df3)
str(df4)
str(df5)
str(df6)
str(df7)
str(df8)
str(df9)
str(df10)
str(df11)
str(df12)

#join all the data together
bikes_rides <-bind_rows(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

bikes_rides

# Rename columns to make them consistent

bikes_rides <-rename(bikes_rides, trip_id =ride_id, bike_type=rideable_type,start_time=started_at, end_time=ended_at,
                     from_station_name=start_station_name, from_station_id=start_station_id,to_station_name=end_station_name,
                     to_station_id=end_station_id,user_type=member_casual)

#cleaning the data

bikes_rides <- clean_names(bikes_rides)  #clean up non consistent variable names

bikes_rides <- remove_empty(bikes_rides, c("rows","cols"))  #remove empty rows and columns

bikes_rides <- subset(bikes_rides, select=-c(start_lat,start_lng,end_lat,end_lng)) #removing columns that are not required for the analysis

#summary statistics on dataset
nrow(bikes_rides)  #number of rows in dataset
head(bikes_rides) #view first 5 rows
summary(bikes_rides)


#Analyse phase
all_trips <- filter(bikes_rides, ride_length>0)
View(all_trips)


#aggregate our data based on the user_type column and find summary statistics
all_trips %>% 
  group_by(user_type) %>% 
  drop_na() %>% 
  summarise(mean_ride_length=mean(ride_length), min_ride_length=min(ride_length), max_ride_length=max(ride_length))

# find the day of week that rides are mostly used
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode_day_of_week <- getmode(all_trips$day_of_week)
mode_day_of_week

#find the day of week thats most common based on user_type
all_trips %>% 
  group_by(user_type) %>% 
  drop_na() %>% 
  summarise(getmode(day_of_week))

#find average ride_length per user_type per day of week and visualize
all_trips %>% 
  group_by(user_type, day_of_week) %>% 
  drop_na() %>% 
  summarise(average_ride=mean(ride_length)) %>% 
  ggplot() + geom_col(aes(x = day_of_week, y = average_ride, fill = user_type), position = "dodge") +
  scale_x_continuous(breaks = seq(0, 7, 1), 
                     limits=c(0, 8)) +
  labs(title = "AVERAGE RIDE LENGTH PER USER_TYPE DAILY", x = "Average ride length", y = "Day of week")+
  ggsave("AVERAGE RIDE LENGTH PER USER_TYPE DAILY.png")

#find the average ride per user_type
all_trips %>% 
 group_by(user_type) %>% 
  drop_na() %>% 
  summarise(average_ride=mean(ride_length)) %>% 
  ggplot()+ geom_col(aes(x=user_type, y= average_ride, fill=user_type))+
  labs(title = "AVERAGE RIDE LENGTH PER USER_TYPE ", x = "User Type", y = "Average ride")+
  ggsave("AVERAGE RIDE LENGTH PER USER_TYPE.png")

#find the number of rides per user_type daily
ride_per_day <-all_trips %>% 
  count(day_of_week, user_type)
ride_per_day   
  
ggplot(data = ride_per_day)+ geom_col(mapping = aes(x=day_of_week, y=n, fill=user_type), position = "dodge")+
  scale_x_continuous(breaks = seq(1, 7, 1), 
                     limits=c(0, 8))+
  labs(title = "Number of Rides Daily per User Type", x = "Day of week", y = "Number of Ride")+
  ggsave("Number of Rides Daily per User Type.png")

#find the types of bikes mostly preferred by differnt user_type
taste <-all_trips %>% 
  count(bike_type, user_type)

taste
ggplot(data = taste)+ geom_col(mapping = aes(x=bike_type, y=n, fill=user_type), position = "dodge")+
  labs(title = "User Bike Preference", x = "Bike Type", y = "Number of User")+
  ggsave("User Bike Preference.png")



















