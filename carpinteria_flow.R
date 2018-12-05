library(tidyverse)
library(lubridate)

carp_flow <- read_csv("carp_creek_flow.csv") %>% mutate(date_time = paste(Date, " ", Time), date_time = mdy_hms(date_time)) %>% rename(flow = Flow) %>% select(date_time, flow) %>% mutate(Day = date(date_time), Year = year(date_time))

daily_flow_summary <- carp_flow %>% group_by(Day) %>% summarize(mean_daily_flow = mean(flow))

annual_flow_summary <- carp_flow %>% group_by(Year) %>% summarize(annual_mean_flow = round(mean(flow, na.rm = TRUE), digits = 0))

all_flow_summary <- annual_flow_summary %>% summarize(low_flow = quantile(annual_flow_summary$annual_mean_flow, probs = .50), high_flow = quantile(annual_flow_summary$annual_mean_flow, probs = 0.90)) %>% View()
