



# relationship between total trips and usertype
graph_pie_total_trips_by_usertype <- 
  chart_circle(
    data = trips_by_usertype, 
    percent_values = trips_by_usertype$percent_of_total_trips, 
    categories = trips_by_usertype$usertype,
    title_lab = "Total Nuber of Trips Broken Down By User Type in 2013",
    fill_lab = "User Type"
  ) 


graph_bar_usertype_vs_num_trips <-  
  chart_bar(
    data = trips_by_usertype, 
    x_value = trips_by_usertype$trips_total,
    y_values = trips_by_usertype$usertype,
    fill_color = "black",
    title_lab = "User Type Vs. Num. Trips in 2013",
    x_lab = "Num. Trips",
    y_lab = "User Type"
  )  + 
  #scale_color_manual(values = c("blue", "red")) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
    #plot.background = element_rect(fill = "lightgray")
  ) + 
  scale_x_continuous(labels = scales::label_comma())


chart_line_total_trips_vs_age_group_for_subscribers <-
  chart_line(
    data = trips_by_age,
    x_value = trips_by_age$age,
    y_value = trips_by_age$subs_trips,
    grouping = 1,
    title_lab = "Num Trips vs. Age Group, Subscribers Only in 2013",
    x_lab = "Age Group",
    y_lab = "Num. Trips",
    color_lab = "User Type"
  )  + 
  #scale_color_manual(values = c("blue", "red")) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
    #plot.background = element_rect(fill = "lightgray")
  )


graph_bar_usertype_vs_num_trips_by_gender <-
  chart_bar_with_multiple_value_per_category(
    data = trips_by_gender_and_usertype,
    x_value = trips_by_gender_and_usertype$trips_total,
    y_value = trips_by_gender_and_usertype$usertype,
    fill_values = trips_by_gender_and_usertype$gender,
    title_lab = "User Type vs. Num. Trips by Gender in 2013",
    x_lab = "Num. Trips",
    y_lab = "User Type",
    fill_lab = "Gender: "
  ) + 
  #scale_fill_manual(values = c("Male" = "blue", "Female" = "red", "NA" = "black")) +
  #scale_fill_brewer(palette = "Set2") +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "top",
    #legend.title = element_blank(),
    panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
    #plot.background = element_rect(fill = "lightgray")
  ) +
  scale_x_continuous(labels = scales::label_comma()) +
  annotate(
    "text",                # Annotation type (text in this case)
    x = 100000,                 # X-coordinate of the annotation
    y = "Casual",                # Y-coordinate of the annotation
    label = "No gender data for 'NA' category", # Text to display
    color = "black",          # Text color
    size = 4                # Text size
  )


graph_bar_usertype_vs_avg_trip_duration_by_gender <-
  chart_bar_with_multiple_value_per_category(
    data = trips_by_gender_and_usertype,
    x_value = trips_by_gender_and_usertype$avg_trip_duration_min,
    y_value = trips_by_gender_and_usertype$usertype,
    fill_values = trips_by_gender_and_usertype$gender,
    title_lab = "User Type vs. Avg. Trip Duration by Gender in 2013",
    x_lab = "Avg. Trip Duration (Min)",
    y_lab = "User Type",
    fill_lab = "Gender"
  ) +
  
  #scale_fill_manual(values = c("Male" = "blue", "Female" = "red", "NA" = "darkgrey")) +
  #scale_fill_brewer(palette = "Set2") +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "right",
    #legend.title = element_blank(),
    panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
    #plot.background = element_rect(fill = "lightgray")
  ) +
  scale_x_continuous(labels = scales::label_comma()) + 
  annotate(
    "text",                # Annotation type (text in this case)
    x = 60,                 # X-coordinate of the annotation
    y = "Subscriber",                # Y-coordinate of the annotation
    label = "No gender data for 'NA' category", # Text to display
    color = "black",          # Text color
    size = 4                # Text size
  )

# relationship between tripduraton and usertype
graph_bar_usertype_vs_avg_trip_duration <-  
  chart_bar(
    data = trips_by_usertype, 
    x_value = trips_by_usertype$avg_trip_duration_min,
    y_value = trips_by_usertype$usertype,
    fill_color = "black",
    title_lab = "User Type Vs. Avg. Trip Duration in 2013",
    x_lab = "Avg. Trip Duration (Mins)",
    y_lab = "User Type"
  ) +
  
  scale_fill_brewer(palette = "Set2") +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
    #plot.background = element_rect(fill = "lightgray")
  ) + 
  scale_x_continuous(labels = scales::label_comma())

  










graph_line_num_trips_vs_month_by_usertype <-
  chart_line(
    data = trips_by_month_and_usertype,
    x_value = trips_by_month_and_usertype$month,
    y_value = trips_by_month_and_usertype$trips_total,
    grouping = trips_by_month_and_usertype$usertype,
    title_lab = "Num. Trips vs Month By User Type in 2013",
    x_lab = "Month",
    y_lab = "Num. Trips",
    color_lab = "User Type"
  ) + 
  #scale_color_manual(values = c("blue", "red")) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
    #plot.background = element_rect(fill = "lightgray")
  )

graph_line_avg_trip_duration_vs_month_by_usertype <-
  chart_line(
    data = trips_by_month_and_usertype,
    x_value = trips_by_month_and_usertype$month,
    y_value = trips_by_month_and_usertype$avg_trip_duration_min,
    grouping = trips_by_month_and_usertype$usertype,
    title_lab = "Avg. Trip Duration vs. Month By User Type in 2013",
    x_lab = "Month",
    y_lab = "Avg. Trip Duration (Min)",
    color_lab = "User Type"
  ) + 
  #scale_color_manual(values = c("blue", "red")) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
    #plot.background = element_rect(fill = "lightgray")
  )




graph_line_num_trips_vs_day_of_week_by_usertype <-
  chart_line(
    data = trips_by_day_of_week_and_usertype,
    x_value = trips_by_day_of_week_and_usertype$start_day_of_week,
    y_value = trips_by_day_of_week_and_usertype$trips_total,
    grouping = trips_by_day_of_week_and_usertype$usertype,
    title_lab = "Num. Trips vs. Day Of Week By User Type in 2013",
    x_lab = "Day of Week",
    y_lab = "Num. Trips",
    color_lab = "User Type"
  ) + 
  #scale_color_manual(values = c("blue", "red")) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
    #plot.background = element_rect(fill = "lightgray")
  )
 
graph_line_avg_trip_duration_vs_day_of_week_by_usertype <-
  chart_line(
    data = trips_by_day_of_week_and_usertype,
    x_value = trips_by_day_of_week_and_usertype$start_day_of_week,
    y_value = trips_by_day_of_week_and_usertype$avg_trip_duration_min,
    grouping = trips_by_day_of_week_and_usertype$usertype,
    title_lab = "Avg. Trip Duration vs. Day Of Week By User Type in 2013",
    x_lab = "Day of Week",
    y_lab = "Avg. Trip Duration (Min)",
    color_lab = "User Type"
  ) + 
  #scale_color_manual(values = c("blue", "red")) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
    #plot.background = element_rect(fill = "lightgray")
  )





graph_line_num_trips_vs_hour_by_usertype <-
  chart_line(
    data = trips_by_hour_and_usertype,
    x_value = trips_by_hour_and_usertype$hour,
    y_value = trips_by_hour_and_usertype$trips_total,
    grouping = trips_by_hour_and_usertype$usertype,
    title_lab = "Num. Trips vs. Hour By User Type in 2013",
    x_lab = "Hour of Day",
    y_lab = "Num. Trips",
    color_lab = "User Type"
  ) + 
  #scale_color_manual(values = c("blue", "red")) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
    #plot.background = element_rect(fill = "lightgray")
  )

graph_line_avg_trip_duration_hour_by_usertype <-
  chart_line(
    data = trips_by_hour_and_usertype,
    x_value = trips_by_hour_and_usertype$hour,
    y_value = trips_by_hour_and_usertype$avg_trip_duration_min,
    grouping = trips_by_hour_and_usertype$usertype,
    title_lab = "Avg. Trip Duration vs. Hour By User Type in 2013",
    x_lab = "Hour of Day",
    y_lab = "Avg. Trip Duration (Min)",
    color_lab = "User Type"
  ) + 
  #scale_color_manual(values = c("blue", "red")) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
    #plot.background = element_rect(fill = "lightgray")
  )




graph_line_num_trips_vs_day_of_week_for_casual_riders_for_hours_01_04 <-
  chart_line(
    data = trips_by_hour_and_usertype_hours_01_04_for_casual_riders,
    x_value = trips_by_hour_and_usertype_hours_01_04_for_casual_riders$start_day_of_week,
    y_value = trips_by_hour_and_usertype_hours_01_04_for_casual_riders$trips_total,
    grouping = 1,
    title_lab = "Num. Trips vs. Day Of Week From 1 AM to 4 AM For Casual Riders in 2013",
    x_lab = "Day Of Week (1 am to 4 am)",
    y_lab = "Num. Trips",
    color_lab = "User Type"
  ) + 
  #scale_color_manual(values = c("blue", "red")) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
    #plot.background = element_rect(fill = "lightgray")
  )




#ggsave("./graphs/graph_pie_total_trips_by_usertype.png", plot = graph_pie_total_trips_by_usertype, device = "png", width = 11, height = 8)
#ggsave("./graphs/graph_bar_usertype_vs_num_trips.png", plot = graph_bar_usertype_vs_num_trips, device = "png", width = 11, height = 8)
#ggsave("./graphs/chart_line_total_trips_vs_age_group_for_subscribers.png", plot = chart_line_total_trips_vs_age_group_for_subscribers, device = "png", width = 11, height = 8)
#ggsave("./graphs/graph_bar_usertype_vs_num_trips_by_gender.png", plot = graph_bar_usertype_vs_num_trips_by_gender, device = "png", width = 11, height = 8)
#ggsave("./graphs/graph_bar_usertype_vs_avg_trip_duration.png", plot = graph_bar_usertype_vs_avg_trip_duration, device = "png", width = 11, height = 8)
#ggsave("./graphs/graph_line_num_trips_vs_month_by_usertype.png", plot = graph_line_num_trips_vs_month_by_usertype, device = "png", width = 11, height = 8)
#ggsave("./graphs/graph_line_avg_trip_duration_vs_month_by_usertype.png", plot = graph_line_avg_trip_duration_vs_month_by_usertype, device = "png", width = 11, height = 8)
#ggsave("./graphs/graph_line_num_trips_vs_day_of_week_by_usertype.png", plot = graph_line_num_trips_vs_day_of_week_by_usertype, device = "png", width = 11, height = 8)
#ggsave("./graphs/graph_line_avg_trip_duration_vs_day_of_week_by_usertype.png", plot = graph_line_avg_trip_duration_vs_day_of_week_by_usertype, device = "png", width = 11, height = 8)
#ggsave("./graphs/graph_line_num_trips_vs_hour_by_usertype.png", plot = graph_line_num_trips_vs_hour_by_usertype, device = "png", width = 11, height = 8)
#ggsave("./graphs/graph_line_avg_trip_duration_hour_by_usertype.png", plot = graph_line_avg_trip_duration_hour_by_usertype, device = "png", width = 11, height = 8)










print(graph_pie_total_trips_by_usertype)
print(graph_bar_usertype_vs_num_trips)

print(graph_bar_usertype_vs_num_trips_by_gender)

print(chart_line_total_trips_vs_age_group_for_subscribers) # remove the causal riders from this group

print(graph_line_num_trips_vs_month_by_usertype)
print(graph_line_num_trips_vs_day_of_week_by_usertype)
print(graph_line_num_trips_vs_hour_by_usertype) 




print(graph_bar_usertype_vs_avg_trip_duration)
#print(graph_bar_usertype_vs_avg_trip_duration_by_gender)


print(graph_line_avg_trip_duration_vs_month_by_usertype)
print(graph_line_avg_trip_duration_vs_day_of_week_by_usertype) 
print(graph_line_avg_trip_duration_hour_by_usertype)
print(graph_line_num_trips_vs_day_of_week_for_casual_riders_for_hours_01_04)