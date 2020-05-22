#Packages to load
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)

original_website <- "http://www.shootingtracker.com/"

mass_shootings_2018 <- read.csv(
  "data/shootings-2018.csv",
  stringsAsFactors = FALSE)

##Summary Info #####################################

total_shootings <- nrow(mass_shootings_2018)

total_lives_lost <- mass_shootings_2018 %>%
  select(num_killed) %>%
  summarize(total = sum(num_killed, na.rm = TRUE)) %>%
  pull(total)

#Most Impacted City - chosen by most people killed
most_impacted_city <- mass_shootings_2018 %>%
  group_by(city) %>%
  summarise(total_killed = sum(num_killed, na.rm = TRUE)) %>%
  filter(total_killed == max(total_killed, na.rm = TRUE)) %>%
  pull(city)

most_impacted_date <- mass_shootings_2018 %>%
  filter(city == most_impacted_city) %>%
  pull(date)

city_w_most_killed <- mass_shootings_2018 %>%
  filter(city == most_impacted_city) %>%
  pull(num_killed)

#Insight 1: state with most people killed
state_w_most_killed <- mass_shootings_2018 %>%
  group_by(state) %>%
  summarise(total_killed = sum(num_killed, na.rm = TRUE)) %>%
  filter(total_killed == max(total_killed, na.rm = TRUE)) %>%
  pull(state)
  
num_killed_state_most_killed <- mass_shootings_2018 %>%
  group_by(state) %>%
  summarise(total_killed = sum(num_killed, na.rm = TRUE)) %>%
  filter(total_killed == max(total_killed, na.rm = TRUE)) %>%
  pull(total_killed)
  
#insight 2: Total number of people injured

total_injured <- mass_shootings_2018 %>%
  select(num_injured) %>%
  summarize(total = sum(num_injured, na.rm = TRUE)) %>%
  pull(total)
  
## Summary Table ###########################################

#Data for Table
summary_table <- mass_shootings_2018 %>%
  group_by(state) %>%
  summarise(
    total_killed = sum(num_killed),
    total_injured = sum(num_injured),
    ms_totals = n()
    ) %>%
  arrange(-ms_totals, -total_killed)

colnames(summary_table) <- c(
  "State",
  "Total Killed",
  "Total Injured",
  "Total Shootings")

#Insights for sum table
sum_table_top_4 <- summary_table$State %>%
  head(4)
sum_table_num_5 <- summary_table[5, 1]
highest_num_shootings <- summary_table[1, 4]

## Desription of a particular incident #####################
#Chosen Incident: murder-suicide - May 16, 2018	Texas	Ponder

incident_info_df <- mass_shootings_2018 %>%
  filter(city == "Ponder")

incident_date <- incident_info_df %>%
  pull(date)

incident_city <- incident_info_df %>%
  pull(city)

incident_state <- incident_info_df %>%
  pull(state)

incident_location <- incident_info_df %>%
  pull(address)

incident_num_killed <- incident_info_df %>%
  pull(num_killed)

incident_num_injured <- incident_info_df %>%
  pull(num_injured)
############################################################
## Interactive Map Info #########################################

text_info <- paste(
  sep = "<br>",
  paste0("City: ", mass_shootings_2018$city),
  paste0("Date: ", mass_shootings_2018$date),
  paste0("Total Kiled: ", mass_shootings_2018$num_killed),
  paste0("Total Injured: ", mass_shootings_2018$num_injured)
)

geo_info <- list(
  scope = "usa"
)

## Plot Data/Info ####################################################
#Get totals for every month(arranged by month using factoring(calendar order))
month_plot_df <- mass_shootings_2018 %>%
  mutate(month = word(date, 1)) %>%
  group_by(month) %>%
  summarise(injured = sum(num_injured), killed = sum(num_killed)) %>%
  mutate(month_abb = substring(month, 0, 3)) %>%
  mutate(month_factor = factor(month_abb, month.abb, ordered = TRUE)) %>%
  arrange(month_factor) #arrange allowed bc of factoring

#Insights about month plot
month_most_injured <- month_plot_df %>%
  filter(injured == max(injured, na.rm = TRUE)) %>%
  pull(month)

month_most_injured_num <- month_plot_df %>%
  filter(injured == max(injured, na.rm = TRUE)) %>%
  pull(injured)
