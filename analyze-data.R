source("my_functions.R")
source("my_globals.R")



#population size = num trips

total_subs <- sum(cyclistic_trips$usertype == "Subscriber")
total_casual_riders <- sum(cyclistic_trips$usertype == "Casual")

male_subs_df <- 
  cyclistic_trips  %>%
  filter(gender == "Male", !is.na(birthday))

# Count the number of matching rows
total_male_subs_with_birthday <- nrow(male_subs_df)
rm(male_subs_df)

#gender_table <- table(cyclistic_trips$gender)
#rm(gender_table)
# Extract the count of 'Male' entries
#total_male_subscribers <- gender_table['Male']
#total_female_subscribers <-  gender_table['Female']

#unequal_dates_count <- sum(cyclistic_trips$start_date != cyclistic_trips$stop_date)

#unequal_dates_rows <- cyclistic_trips[cyclistic_trips$start_date != cyclistic_trips$stop_date, ]

#trips_start_and_end_date_not_same %>% select(cyclistic_trips$start_date != cyclistic_trips$stop_date)


total_check <- total_subs + total_casual_riders
if(total_check == total_rows){
  cat("Analyzing, Looking good so far continuing ...\n")
}
rm(total_check)


casual_riders_with_birthdays <- 
  cyclistic_trips %>% 
  filter(usertype == "Casual" & !is.na(birthday))
casual_riders_with_gender_or_birthdays <- 
  cyclistic_trips %>% 
  filter( usertype == "Casual" & (!is.na(gender) | !is.na(birthday)))



total_tripduration_min <- sum(cyclistic_trips$tripduration_min)

# Pie chart:
#   usertype and percent of trip duration for each
#   usertype and percent of total trips for each
trips_by_usertype = 
  cyclistic_trips %>% 
  group_by(usertype) %>% 
  summarise(
    trips_total = n(),
    tripduration_totals_min    = sum(tripduration_min),
    avg_trip_duration_min      = mean(tripduration_min),
    median_trip_duration_min   = median(tripduration_min),
    sd_trip_duration_min       = sd(tripduration_min),
    varience_trip_duration_min = var(tripduration_min),
    min_trip_duration_min      = min(tripduration_min),
    max_trip_duration_min      = max(tripduration_min),
    
    percent_of_total_trips     = to_percent(trips_total, total_rows),
    percent_of_total_tripduration_min = to_percent(tripduration_totals_min, total_tripduration_min),
  )












# Pie Chart:
#  gender and percent of drip duration for each
#  gender and percent of total trips for each
trips_by_gender <- 
  cyclistic_trips %>% 
  group_by(gender) %>%
  summarise(
    trips_total = n(),
    tripduration_totals_min    = sum(tripduration_min),
    avg_trip_duration_min      = mean(tripduration_min),
    median_trip_duration_min   = median(tripduration_min),
    sd_trip_duration_min       = sd(tripduration_min),
    varience_trip_duration_min = var(tripduration_min),
    min_trip_duration_min      = min(tripduration_min),
    max_trip_duration_min      = max(tripduration_min),
    
    percent_of_total_trips     = to_percent(trips_total, total_rows),
    percent_of_total_tripduration_min = to_percent(tripduration_totals_min, total_tripduration_min),
  )


trips_by_gender_and_usertype = 
  cyclistic_trips %>% 
  group_by(usertype, gender) %>% 
  summarise(
    trips_total = n(),
    tripduration_totals_min    = sum(tripduration_min),
    avg_trip_duration_min      = mean(tripduration_min),
    median_trip_duration_min   = median(tripduration_min),
    sd_trip_duration_min       = sd(tripduration_min),
    varience_trip_duration_min = var(tripduration_min),
    min_trip_duration_min      = min(tripduration_min),
    max_trip_duration_min      = max(tripduration_min),
    
    percent_of_total_trips     = to_percent(trips_total, total_rows),
    percent_of_total_tripduration_min = to_percent(tripduration_totals_min, total_tripduration_min),
    
  )


# Pie Chart:
#  gender and percent of drip duration for each
#  gender and percent of total trips for each
trips_by_gender_drop_na_birthdays <- 
  cyclistic_trips %>% 
  drop_na(birthday) %>% #### some male subscribers have no birthday data
  #filter(age < 90) %>%
  group_by(gender) %>%
  summarise(
    avg_age      = mean( age), 
    median_age   = median(age),
    sd_age       = sd(age),
    varience_age = var(age),
    min_age      = min(age),
    max_age      = max(age),
  )


















trips_by_age_and_usertype <- 
  cyclistic_trips %>% 
  drop_na(birthday) %>%
  group_by(birthday, age, usertype) %>%
  summarise(
    trips_total = n(),
    tripduration_totals_min    = sum(tripduration_min),
    avg_trip_duration_min      = mean(tripduration_min),
    median_trip_duration_min   = median(tripduration_min),
    sd_trip_duration_min       = sd(tripduration_min),
    varience_trip_duration_min = var(tripduration_min),
    min_trip_duration_min      = min(tripduration_min),
    max_trip_duration_min      = max(tripduration_min),
    
    percent_of_total_trips     = to_percent(trips_total, total_rows),
    percent_of_total_tripduration_min = to_percent(tripduration_totals_min, total_tripduration_min),
  ) %>%
  arrange(age)



# for scatter plots
trips_by_age <- 
  cyclistic_trips %>% 
  drop_na(birthday) %>%
  group_by(birthday, age) %>%
  summarise(
    trips_total = n(),
    tripduration_totals_min    = sum(tripduration_min),
    avg_trip_duration_min      = mean(tripduration_min),
    median_trip_duration_min   = median(tripduration_min),
    sd_trip_duration_min       = sd(tripduration_min),
    varience_trip_duration_min = var(tripduration_min),
    min_trip_duration_min      = min(tripduration_min),
    max_trip_duration_min      = max(tripduration_min),
    subs_trips = sum(usertype == "Subscriber"),
    
    percent_of_total_trips     = to_percent(trips_total, total_rows),
    percent_of_total_tripduration_min = to_percent(tripduration_totals_min, total_tripduration_min),
  ) %>%
  arrange(age)




















trips_per_day_and_usertype <- 
  cyclistic_trips %>% 
  group_by(start_date, usertype) %>%
  summarise(
    trips_total = n(),
    tripduration_totals_min    = sum(tripduration_min),
    avg_trip_duration_min      = mean(tripduration_min),
    median_trip_duration_min   = median(tripduration_min),
    sd_trip_duration_min       = sd(tripduration_min),
    varience_trip_duration_min = var(tripduration_min),
    min_trip_duration_min      = min(tripduration_min),
    max_trip_duration_min      = max(tripduration_min),
    
    percent_of_total_trips     = to_percent(trips_total, total_rows),
    percent_of_total_tripduration_min = to_percent(tripduration_totals_min, total_tripduration_min),
    
  ) %>% 
  arrange(start_date)

# day vs trips
# day vs sub_trips vs non_sub_trips
# day vs percent of total trips
# day vs avg_trip_duration
trips_by_day_of_week_and_usertype <- 
  cyclistic_trips %>% 
  group_by(start_day_of_week, usertype) %>%
  summarise(
    trips_total = n(),
    tripduration_totals_min    = sum(tripduration_min),
    avg_trip_duration_min      = mean(tripduration_min),
    median_trip_duration_min   = median(tripduration_min),
    sd_trip_duration_min       = sd(tripduration_min),
    varience_trip_duration_min = var(tripduration_min),
    min_trip_duration_min      = min(tripduration_min),
    max_trip_duration_min      = max(tripduration_min),
    
    percent_of_total_trips     = to_percent(trips_total, total_rows),
    percent_of_total_tripduration_min = to_percent(tripduration_totals_min, total_tripduration_min),
    
  ) %>% 
  arrange(start_day_of_week, usertype)




trips_by_month_and_usertype <- 
  cyclistic_trips %>% 
  mutate(month = format(start_date, "%Y-%m")) %>%
  group_by(month, usertype) %>%
  summarise(
    trips_total = n(),
    tripduration_totals_min    = sum(tripduration_min),
    avg_trip_duration_min      = mean(tripduration_min),
    median_trip_duration_min   = median(tripduration_min),
    sd_trip_duration_min       = sd(tripduration_min),
    varience_trip_duration_min = var(tripduration_min),
    min_trip_duration_min      = min(tripduration_min),
    max_trip_duration_min      = max(tripduration_min),
    
    percent_of_total_trips     = to_percent(trips_total, total_rows),
    percent_of_total_tripduration_min = to_percent(tripduration_totals_min, total_tripduration_min),
    
  ) %>% 
  arrange(month)


# Group by hour
trips_by_hour_and_usertype <- 
  cyclistic_trips %>%
  group_by(hour = format(starttime, format = "%H"), usertype) %>%
  summarize(
    trips_total = n(),
    tripduration_totals_min    = sum(tripduration_min),
    avg_trip_duration_min      = mean(tripduration_min),
    median_trip_duration_min   = median(tripduration_min),
    sd_trip_duration_min       = sd(tripduration_min),
    varience_trip_duration_min = var(tripduration_min),
    min_trip_duration_min      = min(tripduration_min),
    max_trip_duration_min      = max(tripduration_min),
    
    percent_of_total_trips     = to_percent(trips_total, total_rows),
    percent_of_total_tripduration_min = to_percent(tripduration_totals_min, total_tripduration_min),
    
    
  ) %>% 
  arrange(hour)




trips_by_hour_and_usertype_hours_01_04_for_casual_riders <- 
  cyclistic_trips %>%
  mutate(hour = format(starttime, format = "%H")) %>%
  group_by( start_day_of_week) %>%
  filter(hour >= "01" & hour <= "04" & usertype == "Casual") %>%
  summarize(
    trips_total = n(),
    tripduration_totals_min    = sum(tripduration_min),
    avg_trip_duration_min      = mean(tripduration_min),
    median_trip_duration_min   = median(tripduration_min),
    sd_trip_duration_min       = sd(tripduration_min),
    varience_trip_duration_min = var(tripduration_min),
    min_trip_duration_min      = min(tripduration_min),
    max_trip_duration_min      = max(tripduration_min),
    
    #subs_trips = sum(usertype == "Subscriber"), 
    #casual_rider_trips = sum(usertype == "Casual"), 
    
    percent_of_total_trips     = to_percent(trips_total, total_rows),
    percent_of_total_tripduration_min = to_percent(tripduration_totals_min, total_tripduration_min),
    
    
  ) 













trips_by_from_station_name <-
  cyclistic_trips %>% 
  group_by(from_station_name, from_station_id) %>% 
  summarise(
    trips = n(), 
    percent_of_total_trips = to_percent(trips, total_rows),
    avg_trip_duration_min = mean(tripduration_min),
    median_trip_duration_min = median(tripduration_min),
    sd_trip_duration_min = sd(tripduration_min),
    min_trip_duration_min = min(tripduration_min),
    max_trip_duration_min = max(tripduration_min),
    
    subs_trips = sum(usertype == "Subscriber"), 
    subs_to_all_subs_percent = to_percent(subs_trips, total_subs),
    subs_total_trips_percent = to_percent(subs_trips, total_rows),
    
    
    casual_rider_trips = sum(usertype == "Casual"), 
    casual_rider_to_all_casual_riders_percent = to_percent(casual_rider_trips, total_casual_riders),
    casual_rider_to_total_trips_percent = to_percent(casual_rider_trips, total_rows),
    
  ) %>%
  arrange(from_station_name)


trips_by_to_station_name <-
  cyclistic_trips %>% 
  group_by(to_station_name, to_station_id) %>% 
  summarise(
    trips = n(), 
    percent_of_total_trips = to_percent(trips, total_rows),
    avg_trip_duration_min = mean(tripduration_min),
    median_trip_duration_min = median(tripduration_min),
    sd_trip_duration_min = sd(tripduration_min),
    min_trip_duration_min = min(tripduration_min),
    max_trip_duration_min = max(tripduration_min),
    
    subs_trips = sum(usertype == "Subscriber"), 
    subs_to_all_subs_percent = to_percent(subs_trips, total_subs),
    subs_total_trips_percent = to_percent(subs_trips, total_rows),
    
    
    casual_rider_trips = sum(usertype == "Casual"), 
    casual_rider_to_all_casual_riders_percent = to_percent(casual_rider_trips, total_casual_riders),
    casual_rider_to_total_trips_percent = to_percent(casual_rider_trips, total_rows),
    
  ) %>%
  arrange(to_station_name)














trips_by_bikeID <-
  cyclistic_trips %>% 
  group_by(bikeid, usertype) %>% 
  summarise(
    trips_total = n(),
    tripduration_totals_min    = sum(tripduration_min),
    avg_trip_duration_min      = mean(tripduration_min),
    median_trip_duration_min   = median(tripduration_min),
    sd_trip_duration_min       = sd(tripduration_min),
    varience_trip_duration_min = var(tripduration_min),
    min_trip_duration_min      = min(tripduration_min),
    max_trip_duration_min      = max(tripduration_min),
    
    percent_of_total_trips     = to_percent(trips_total, total_rows),
    percent_of_total_tripduration_min = to_percent(tripduration_totals_min, total_tripduration_min),
    
  ) %>%
  arrange(bikeid)



















# Define time intervals (in minutes and hours)
time_intervals <- data.frame(
  intervalID = 1:25,
  start_min  =   c(min_to_sec(0), min_to_sec(5),  min_to_sec(10), min_to_sec(15), min_to_sec(20), min_to_sec(25), min_to_sec(30), min_to_sec(35), min_to_sec(40), min_to_sec(45), min_to_sec(50), min_to_sec(55), min_to_sec(60), min_to_sec(65), min_to_sec(70), min_to_sec(75), min_to_sec(80), min_to_sec(85), min_to_sec(90), min_to_sec(95),  min_to_sec(100), min_to_sec(105), min_to_sec(110), min_to_sec(115), min_to_sec(120)),
  end_min    =   c(min_to_sec(5), min_to_sec(10), min_to_sec(15), min_to_sec(20), min_to_sec(25), min_to_sec(30), min_to_sec(35), min_to_sec(40), min_to_sec(45), min_to_sec(50), min_to_sec(55), min_to_sec(60), min_to_sec(65), min_to_sec(70), min_to_sec(75), min_to_sec(80), min_to_sec(85), min_to_sec(90), min_to_sec(95), min_to_sec(100), min_to_sec(105), min_to_sec(110), min_to_sec(115), min_to_sec(120), min_to_sec(125)),
  
  start_hour =   c(hours_to_sec(0),   hours_to_sec(0.5), hours_to_sec(1),   hours_to_sec(1.5), hours_to_sec(2),   hours_to_sec(2.5), hours_to_sec(3),   hours_to_sec(3.5), hours_to_sec(4),   hours_to_sec(4.5), hours_to_sec(5),   hours_to_sec(5.5), hours_to_sec(6),   hours_to_sec(6.5), hours_to_sec(7),   hours_to_sec(7.5), hours_to_sec(8),   hours_to_sec(8.5), hours_to_sec(9),   hours_to_sec(9.5), hours_to_sec(10),   hours_to_sec(10.5), hours_to_sec(11),   hours_to_sec(11.5), hours_to_sec(12)),
  end_hour   =   c(hours_to_sec(0.5), hours_to_sec(1),   hours_to_sec(1.5), hours_to_sec(2),   hours_to_sec(2.5), hours_to_sec(3),   hours_to_sec(3.5), hours_to_sec(4),   hours_to_sec(4.5), hours_to_sec(5),   hours_to_sec(5.5), hours_to_sec(6),   hours_to_sec(6.5), hours_to_sec(7),   hours_to_sec(7.5), hours_to_sec(8),   hours_to_sec(8.5), hours_to_sec(9),   hours_to_sec(9.5), hours_to_sec(10),  hours_to_sec(10.5), hours_to_sec(11),   hours_to_sec(11.5), hours_to_sec(12),   hours_to_sec(12.5))
)

# Create a data frame for time intervals and initialize the count to 0
result_df <- 
  data.frame(
    #intervalID      = rep(time_intervals$intervalID,      each = nrow(cyclistic_trips)),
    start_min       = rep(time_intervals$start_min,       each = nrow(cyclistic_trips)),
    end_min         = rep(time_intervals$end_min,         each = nrow(cyclistic_trips)),
    
    start_hour      = rep(time_intervals$start_hour,      each = nrow(cyclistic_trips)),
    end_hour        = rep(time_intervals$end_hour,        each = nrow(cyclistic_trips)),
    Count = 0
  )


# Compare each ride duration to the time intervals and count matches
trips_between_intervals <- 
  result_df %>%
  group_by(start_min, end_min, start_hour, end_hour) %>%
  summarise(
    trips_between_start_and_end_mins = sum(cyclistic_trips$tripduration_sec >= start_min & cyclistic_trips$tripduration_sec < end_min),
    trips_between_start_and_end_hour = sum(cyclistic_trips$tripduration_sec >= start_hour & cyclistic_trips$tripduration_sec < end_hour)
    ) %>%
  mutate(start_min = sec_to_min(start_min)) %>%
  mutate(end_min = sec_to_min(end_min)) %>%
  
  mutate(start_hour = sec_to_hours(start_hour)) %>%
  mutate(end_hour = sec_to_hours(end_hour))

rm(result_df)
rm(time_intervals)

