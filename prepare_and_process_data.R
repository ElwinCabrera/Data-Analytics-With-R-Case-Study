source("my_functions.R")
source("my_globals.R")


#cyclistic_stations_2013 <- read_csv("/home/elwin/Desktop/Google-Data-Analytics-Professional-Certificate/case-study/Divvy_Stations_Trips_2013/Divvy_Stations_2013.csv")
cyclistic_trip_data_2013 <- read_csv("/home/elwin/Desktop/Google-Data-Analytics-Professional-Certificate/case-study/Divvy_Stations_Trips_2013/Divvy_Trips_2013.csv")

total_rows <- nrow(cyclistic_trip_data_2013)

str(cyclistic_trip_data_2013)
head(cyclistic_trip_data_2013)
skim_without_charts(cyclistic_trip_data_2013)
glimpse(cyclistic_trip_data_2013)



cyclistic_trips <- cyclistic_trip_data_2013

cyclistic_trips$usertype <- gsub("Customer", "Casual", cyclistic_trips$usertype)

cyclistic_trips$usertype <- factor(cyclistic_trips$usertype, ordered = FALSE, levels = c("Casual", "Subscriber"))
cyclistic_trips$gender <- factor(cyclistic_trips$gender, ordered = FALSE, levels = c("Male", "Female", "NA"))

# trim leadin and trailing white space
#cyclistic_trips <- 
#  cyclistic_trips %>% 
#  lapply(trimws)

# Check for duplicate rows in the data frame
#duplicate_rows <- duplicated(cyclistic_trips) | duplicated(cyclistic_trips, fromLast = TRUE)

#cyclistic_trips <- cyclistic_trips[!duplicate_rows, ]


cyclistic_trips  <- 
  cyclistic_trips %>%
  separate(starttime, into=c('start_date', 'start_time'), sep=" ", remove = FALSE) %>% 
  separate(stoptime, into=c('stop_date', 'stop_time'), sep=" ", remove = FALSE) %>% 
  mutate(start_time = ifelse(is.na(start_time), "00:00:00", start_time)) %>%
  mutate(stop_time = ifelse(is.na(stop_time), "00:00:00", stop_time)) %>%
  mutate(start_time = as.character(start_time)) %>%
  mutate(start_date = as.Date(start_date)) %>%
  mutate(stop_time = as.character(stop_time)) %>%
  mutate(stop_date = as.Date(stop_date)) 

  
  

cyclistic_trips  <- 
  cyclistic_trips %>%
  mutate(stop_time = add_seconds_to_time(start_time, tripduration)) %>%
  mutate(age = ifelse(is.na(birthday), NA, 2013 - birthday)) %>%
  mutate(start_day_of_week = weekdays(start_date, abbreviate = TRUE)) %>%
  #mutate(start_day_of_week = factor(start_date, levels=c('MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT', 'SUN'), ordered = TRUE))
  mutate(is_weekend = ifelse(start_day_of_week %in% c("Sat", "Sun"), TRUE, FALSE)) %>%
  mutate(tripduration_min = tripduration/60) %>%
  rename(tripduration_sec = tripduration) %>%
  mutate(
    start_day_of_week = factor(weekdays(start_date, abbreviate = TRUE), 
                               levels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'),
                               ordered = TRUE)
  )
#cyclistic_trips <- 
#  separate(cyclistic_trips, stoptime, into=c('stop_date', 'stop_time'), sep=' ') %>% 
#  mutate(stop_date = as.Date(stop_date))

#cyclistic_trips <- 
#  mutate(age = ifelse(is.na(birthday), NA, today_year - birthday)) %>%
#  mutate(start_day_of_week = weekdays(start_date)) %>%
#  mutate(is_weekend = ifelse(start_day_of_week %in% c("Saturday", "Sunday"), TRUE, FALSE))
cyclistic_trips <- 
  cyclistic_trips %>%
  mutate(start_time = as.POSIXct(start_time, format = "%H:%M:%S")) %>%
  mutate(stop_time = as.POSIXct(stop_time, format = "%H:%M:%S"))
  
clean_names(cyclistic_trips)

str(cyclistic_trips)
head(cyclistic_trips)
skim_without_charts(cyclistic_trips)
glimpse(cyclistic_trips)


total_rows_clean <- nrow(cyclistic_trips)
if(total_rows != total_rows_clean){
  cat("WARNING: Cleaned dataset row count dont match with original dataset.\n")
} else {
  cat("Row count of the newly cleaned dataset matches the original!\n")
}

rm(total_rows_clean)

#total_check <- num_subscribers + num_non_subscribers
#rm(total_check)




#cyclistic_trip_data_2013 %>% select(stoptime)
#cyclistic_trip_data_2013 %>% select(-starttime)
#cyclistic_trip_data_2013 %>% arrange(-tripduration)
#cyclistic_trip_data_2013 %>% filter(usertype == "Subscriber")

na_start_time_values <- 
  cyclistic_trips %>% 
  filter(is.na(start_time))
na_stop_time_values <- 
  cyclistic_trips %>% 
  filter(is.na(stop_time))

all_na_times <- 
  full_join(na_start_time_values, na_stop_time_values, by = "trip_id")


# Inner join (matching IDs)
inner_merged_cyclistic_trip_data_2013_and_na_times <- inner_join(cyclistic_trip_data_2013, all_na_times, by = "trip_id")

is_empty <- nrow(na_start_time_values) == 0  && nrow(na_stop_time_values) == 0 
if(is_empty){
  rm(na_start_time_values)
  rm(na_stop_time_values)
  rm(all_na_times)
  rm(inner_merged_cyclistic_trip_data_2013_and_na_times)
  rm(is_empty)
  cat("Dataset looks good!\n")
} else {
  cat("WARNING: Cleaned dataset has NULL or NA values\n")
}

