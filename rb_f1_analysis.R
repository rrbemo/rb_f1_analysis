options(scipen=999)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(yaml)

#api_key <- yaml.load_file("api_key.yaml")$api_key

ms_to_datetime <- function(ms, tz = "UTC", origin = "1970-01-01") {
  sec <- ms / 1000
  as.POSIXct(sec, tz = tz, origin = origin)
}

base_url <- "http://127.0.0.1:5000"
page_events <- "fast_f1_events_all"
page_sessions <- "fast_f1_sessions"
page_weather <- "fast_f1_weather"
page_laps <- "fast_f1_laps"
page_pos <- "fast_f1_pos_data"
page_timezones <- "fast_f1_timezones"
#year <- "2022"
#event <- "Bahrain Grand Prix"
#session <- "Race"
year <- "2023"
event <- "Pre-Season Testing"
session <- "1"

url_timezones <- URLencode(paste0(base_url, "/", page_timezones))
timezones <- fromJSON(url_timezones)

url_events <- URLencode(paste0(base_url, "/", page_events, "/", year))
events <- fromJSON(url_events) %>%
  left_join(timezones, by = c("Country", "Location")) %>%
  rowwise() %>%
  mutate(EventDate = ms_to_datetime(EventDate, Timezone),
         Session1Date = ms_to_datetime(Session1Date, Timezone),
         Session2Date = ms_to_datetime(Session2Date, Timezone),
         Session3Date = ms_to_datetime(Session3Date, Timezone),
         Session4Date = ms_to_datetime(Session4Date, Timezone),
         Session5Date = ms_to_datetime(Session5Date, Timezone)) %>%
  ungroup()

url_sessions <- URLencode(paste0(base_url, "/", page_sessions, "/", year, "/", event))
sessions <- fromJSON(url_sessions)
sessions <- sessions %>%
  left_join(timezones, by = c("Country", "Location")) %>%
  rowwise() %>%
  mutate(SessionDate = ms_to_datetime(SessionDate, Timezone)) %>%
  ungroup

url_weather <- URLencode(paste0(base_url, "/", page_weather, "/", year, "/", event, "/", session))
weather <- fromJSON(url_weather)

url_laps <- URLencode(paste0(base_url, "/", page_laps, "/", year, "/", event, "/", session))
#url_laps <- URLencode(paste0(base_url, "/", page_laps, "/2022/Bahrain Grand Prix/FP1"))
laps <- fromJSON(url_laps)

url_pos <- URLencode(paste0(base_url, "/", page_pos, "/", year, "/", event, "/", session))
pos_data <- fromJSON(url_pos)

