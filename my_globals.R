if (!exists("MY_GLOBALS_R")) {
  #source("script2.R")
  MY_GLOBALS_R <- TRUE



  #install.packages("tidyverse")
  #install.packages("lubridate")
  #install.packages("ggplot2")
  #install.packages("gridExtra")
  
  #update.packages()
  library(lubridate)
  library(skimr)
  library(tidyr)
  library(dplyr)
  library(janitor)
  library(readr)
  library(ggplot2)
  #library(gridExtra)

  total_rows <- 0
  # Get today's date
  today_date <- Sys.Date()

  # Extract the year from today's date
  today_year <- as.numeric(format(today_date, "%Y"))


  # Define a custom function to add seconds to time
  one_day_in_seconds <- 60 * 60 * 24


}  # END_IF