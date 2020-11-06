library(tidyverse)
covid <- read_csv('https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv')
write_csv(covid, 'covid.csv')


# Malaysia data from dataworld
library("httr")
library("readxl")
GET("https://query.data.world/s/dl2knkmq7y2erjews5hnsc6vjbr3pc", 
    write_disk(tf <- tempfile(fileext = ".xlsx")))
mys_dw <- read_excel(tf, sheet = 'd(Base)') %>% clean_names() %>%
  select(Time = date, mco_phase, cum_pos = positive, I = d_positive) %>%
  mutate_if(is.POSIXt, as.Date) %>% 
  filter(Time < '2020-06-07') %>%
  data.frame()
mys_dw_state <- read_excel(tf, sheet = 'State') %>% clean_names() %>%
  select(Time = date, perlis:total, east_malaysia, west_malaysia) %>%
  mutate_if(is.POSIXt, as.Date) %>% filter(Time > '2020-02-3') %>%
  data.frame()