# Sara McLaughlin
# 02/17/2025
# Saving code for COVID-19 data analysis - daily assignment 7
library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)
install.packages("dplyr")
library("dplyr")
library("ggplot2")

top_states <- covid %>% filter(date == max(date)) %>% group_by(state) %>% mutate(cumulative_cases = cumsum(cases)) %>% summarize(total_cases = sum(cases, na.rm = TRUE)) %>% arrange(desc(total_cases)) %>% slice_head(n = 6) %>% pull(state)
print(top_states)
filtered_covid <- covid %>% filter(state %in% top_states) %>% group_by(state, date) %>% summarize(totCase = sum(cases))
ggplot(filtered_covid, aes(x = date, y = totCase, group = state, color = state)) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  labs(title = "COVID-19 Total Cases in 6 Worst States",
       x = "Date", 
       y = "Cases") +
  theme_minimal() +
  theme(legend.position = "none")

covid$cases <- as.numeric(covid$cases)
cases_bydate <- covid %>% group_by(date) %>% summarise(total_cases = sum(cases))
ggplot(cases_bydate, aes(x = date, y = total_cases)) +
  geom_col(fill = "orange") +
  labs(title = "COVID-19 Nationwide Total Cases",
       x = "Date", 
       y = "Cases") +
  theme_minimal()

                     