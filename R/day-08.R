# Sara McLaughlin
# 02-19-2025
# Code for making a faceted plot for COVID-19 data

url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)
df = data.frame(region = state.region,
                abbreviation = state.abb,
                state = state.name)
merged_data <- merge(covid, df, by = "state", all.x = FALSE, all.y = FALSE)
install.packages("dplyr")
library(dplyr)
library(tidyverse)

by_region <- merged_data %>% group_by(region, date) %>% summarize(total_cases = sum(cases), 
                                                                  total_deaths = sum(deaths),
                                                                  .groups = 'drop') %>% 
  arrange(region, date) %>% group_by(region) %>% mutate(daily_cases = cumsum(total_cases),
                                   daily_deaths = cumsum(total_deaths))

long_data <- by_region %>% pivot_longer(cols = c(daily_cases, daily_deaths), names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric, daily_cases = "Cases", daily_deaths = "Deaths"))

ggplot(long_data, aes(x = date, y = value, color = region)) +
  geom_line(size = 1) +
  facet_grid(metric ~ region, scale = "free_y") +
  labs(title = "Cumulative Cases and Deaths by Region",
       x = "Date",
       y = "Daily Cumulative Count") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b") +
  theme_minimal() +
  theme(legend.position = "none")

