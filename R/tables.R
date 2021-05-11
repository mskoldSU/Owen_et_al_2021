library(tidyverse)
source("R/functions.R")

##
## Data
##

daily_data <- read_csv("clean_data/daily_data.csv", col_types = cols(station = col_character())) %>% 
  mutate(study = ifelse(lubridate::year(date) < 2015, "SAMBAH", "SNMP")) %>% 
  filter(remove == FALSE)

# Stations, years and season for indices
index_stations <- c("1032", "1041", "1036")
index_years <- c(2011, 2012, 2017, 2018, 2019)
index_season <- 5:10 # May-Oct



##
## Summary table of data, table 1
##

data_table <- summary_table(daily_data) %>% 
  left_join(summary_table(filter(daily_data, lubridate::month(date) %in% index_season)), 
            by = "station", 
            suffix = c("", ".summer"))


##
## Missing data table, table S1
##

missing_table <- daily_data %>% 
  mutate(year = lubridate::year(date), month = lubridate::month(date)) %>% 
  filter(year %in% c(2011, 2012, 2017, 2018, 2019), month %in% 5:10) %>% 
  group_by(station, year) %>% 
  summarise(cens = round(pmax(0, 1-n()/183)*100), .groups = "drop") %>% 
  pivot_wider(names_from = year, values_from = cens, values_fill = 100)
  

##
## Trends tables, table 2
##

responses <- c("dph", "dps", "n_clicks", "n_encounters", "n_trains")
index_data <- map(responses, ~make_indices(index_stations, index_years, index_season, response = .x))
coeff_tables <- map(index_data, coeff_table)


##
## Save tables
##

write_csv(data_table, file = "tables/table_1.csv")
write_csv(coeff_tables[[1]], file = "tables/table_2.csv")
write_csv(missing_table, file = "tables/table_S1.csv")


