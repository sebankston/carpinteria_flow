library(tidyverse)
library(lubridate)

carp_flow <- read_csv("carp_creek_flow.csv") %>% mutate(date_time = paste(Date, " ", Time), date_time = mdy_hms(date_time)) %>% rename(flow = Flow) %>% select(date_time, flow) %>% mutate(Day = date(date_time), Year = year(date_time))

daily_flow_summary <- carp_flow %>% filter(month(date_time) >= 1 & month(date_time) < 7) %>% group_by(Year, Day) %>% summarize(mean_daily_flow = mean(flow, na.rm = TRUE)) %>% ungroup()

annual_flow_summary <- daily_flow_summary %>% group_by(Year) %>% summarize(annual_mean_flow = round(mean(mean_daily_flow, na.rm = TRUE), digits = 0))

all_flow_summary <- daily_flow_summary %>% summarize(low_flow = quantile(daily_flow_summary$mean_daily_flow, probs = .50, na.rm = TRUE), high_flow = quantile(daily_flow_summary$mean_daily_flow, probs = 0.99, na.rm = TRUE)) %>% print()

num_years <- n_distinct(annual_flow_summary$Year)
