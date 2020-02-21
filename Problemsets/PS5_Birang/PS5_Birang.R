library(rvest)
library(tidyverse)
library(janitor)
library(lubridate)
m100 <- read_html("https://en.wikipedia.org/wiki/ISO_3166-1") 
m100

m100 %>%
  html_nodes("#mw-content-text > div > table:nth-child(30)") %>%
  html_table(fill=TRUE) 
pre_iaaf <-
  m100 %>%
  html_nodes("#mw-content-text > div > table:nth-child(30)") %>%
  html_table(fill=TRUE) 
class(pre_iaaf)

pre_iaaf <-
  pre_iaaf %>%
  clean_names()
pre_iaaf

pre_iaaf <- 
  pre_iaaf %>%
  bind_rows() %>%
  as_tibble()
pre_iaaf
result_df = data.frame(name = character(),property = character(),value = numeric())
write.csv(result_df,file = "GDP.csv")


#Question4
library(fredr)
library(dplyr)
library(ggplot2)

fredr_set_key("e8faa65070a79d54c93f0b36275ce112")
fredr(series_id="GDPC1", observation_start= as.Date("1960-01-01"))
fredr_series_observations(
  series_id = "GDPC1",
  observation_start = as.Date("1960-01-01"),
  frequency = "q",
  units = "chg"
)

library(purrr)
map_dfr(c("GDPC1"), fredr) %>%
  ggplot(data = ., mapping = aes(x = date, y = value, color = series_id)) +
  geom_line() +
  labs(x = "Observation Date", y = "Rate", color = "Series")



