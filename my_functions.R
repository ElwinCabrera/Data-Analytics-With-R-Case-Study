if (!exists("MY_FUNCTIONS_R")) {
  #source("script2.R")
  MY_FUNCTIONS_R <- TRUE


  # Define a custom function to add seconds to time
  add_seconds_to_time <- function(time_str, seconds) {
    time_obj <- as.POSIXct(time_str, format = "%H:%M:%S")
    new_time_obj <- time_obj + lubridate::seconds(seconds)
    return(format(new_time_obj, format = "%H:%M:%S"))
  }

  to_percent <- function(value, total) {
    result <- (value / total) * 100
    return(result)
  }

  min_to_sec <- function(mins) {
    return(mins * 60)
  }
  
  hours_to_sec <- function(hour){
    return(hour * 60 * 60)
  }
  days_to_sec <- function(days){
    return(days * 24 * 60 * 60)
  }
  
  sec_to_min <- function(sec) {
    return(sec / 60)
  }
  
  sec_to_hours <- function(sec){
    result_min <- sec_to_min(sec)
    result <- result_min / 60
    return(result)
  }
  sec_to_days <- function(sec){
    result_hour <- sec_to_hours(sec)
    result <- result_hour / 24
    return(result)
  }
  
  hours_to_min <- function(hour){
    return(hour * 60)
  }
  days_to_min <- function(days){
    return(days * 24 * 60)
  }
  
  chart_circle <- function(data, percent_values, categories, title_lab, fill_lab) {
    pie_chart <- 
      ggplot(data, aes(x = "", y = percent_values, fill = categories)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() + 
      labs(title = title_lab, fill = fill_lab) + 
      geom_text(aes(label = paste0(round(percent_of_total_trips), "%")), position = position_stack(vjust = 0.5))
    
    return(pie_chart)
  }
  
  chart_bar <- function(data, x_values, y_values, fill_color, title_lab, x_lab, y_lab) {
    # X =Categories 
    # Y = Values
    bar_chart <- 
      ggplot(data, aes(x = x_values, y = y_values)) +
      geom_bar(stat = "identity", width = 0.7, fill = fill_color) +
      labs(title = title_lab, x =  x_lab, y = y_lab)
    return(bar_chart)
  }
  
  chart_bar_with_multiple_value_per_category <- function(data, x_value, y_value, fill_values, title_lab, x_lab, y_lab, fill_lab) {
    bar_graph <- 
      ggplot(data, aes(x = x_value, y = y_value, fill = fill_values)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.6) +
      labs(title = title_lab, x =  x_lab, y = y_lab, fill = fill_lab) #+
      #scale_fill_manual(values = c("Male" = "red", "Female" = "green", "NA" = "blue")) +
      #scale_fill_brewer(palette = "Set2") +
      #theme(
        #axis.text.x = element_text(size = 12),
        #axis.text.y = element_text(size = 12),
        #legend.position = "top",
        #legend.title = element_blank(),
        #panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
        #plot.background = element_rect(fill = "lightgray")
      #)
    return(bar_graph)
  }
  
  
  chart_scatter <- function(data, x_values, y_values, fill_color, point_size, title_lab, x_lab, y_lab) {
    # X =Categories 
    # Y = Values
    scatter_chart <- 
      ggplot(data, aes(x = x_values, y = y_values)) +
      geom_point(size = point_size, color = fill_color) +
      #geom_smooth(method = "lm") + # Add a linear trend line
      #theme_minimal() +
      labs(title = title_lab, x =  x_lab, y = y_lab) 
    return(scatter_chart)
  }
  
  
  chart_line <- function(data, x_value, y_value, grouping, title_lab, x_lab, y_lab, color_lab){
    ggplot_func <- NA
    if(is.numeric(grouping)) {
      ggplot_func <- ggplot(data, aes(x = x_value, y = y_value, group = grouping))
    } else {
      ggplot_func <- ggplot(data, aes(x = x_value, y = y_value, group = grouping, color = grouping))
    }
    line_graph <- 
      #ggplot(data, aes(x = x_value, y = y_value, group = grouping, color = grouping)) +
      ggplot_func +
      geom_line()+
      theme_minimal()+
      labs(title = title_lab, x = x_lab, y = y_lab, color = color_lab)
    
    return(line_graph)
    
  }
  

} # End_IF





# Define time intervals (in minutes)
#time_intervals <- c( 0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
#                     min_to_sec(1), min_to_sec(2), min_to_sec(3),
#                     min_to_sec(4), min_to_sec(5), min_to_sec(10), min_to_sec(15), 
#                     min_to_sec(20), min_to_sec(25), min_to_sec(30), 
#                     min_to_sec(35), min_to_sec(40), min_to_sec(45), 
#                     min_to_sec(50), min_to_sec(55), hours_to_sec(1), 
#                     hours_to_sec(2), hours_to_sec(3), hours_to_sec(4), 
#                     hours_to_sec(5), hours_to_sec(6), hours_to_sec(7), 
#                     hours_to_sec(8), hours_to_sec(9), hours_to_sec(10),
#                     hours_to_sec(11), hours_to_sec(12), hours_to_sec(13),
#                     hours_to_sec(14),hours_to_sec(15),hours_to_sec(16),
#                     hours_to_sec(17),hours_to_sec(18),hours_to_sec(19),
#                     hours_to_sec(20),hours_to_sec(21),hours_to_sec(22),
#                     hours_to_sec(23), hours_to_sec(24))
#
# Create a data frame with all possible combinations of Duration and time_intervals
#combinations_df <- expand.grid(duration = cyclistic_trips$tripduration_sec, interval = time_intervals)

# Count the number of rows that match each combination
#trips_lasting_more_than_XSec <- 
#  combinations_df %>%
#  group_by(interval) %>%
#  summarize(count = sum(duration >= interval))

#trips_lasting_less_than_XSec <- 
#  combinations_df %>%
#  group_by(interval) %>%
#  summarize(count = sum(duration <= interval))

# Rename the columns for clarity
#colnames(result_df) <- c("Time_Interval", "Count_of_Rows")
#rm(combinations_df)