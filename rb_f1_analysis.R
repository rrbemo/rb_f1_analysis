options(scipen=999)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(yaml)
library(plotly)

#api_key <- yaml.load_file("api_key.yaml")$api_key

ms_to_datetime <- function(ms, tz = "UTC", origin = "1970-01-01") {
  sec <- ms / 1000
  as.POSIXct(sec, tz = tz, origin = origin)
}

ms_to_duration <- function(ms) {
  dmilliseconds(ms)
}

add_my_stint <- function(laps_data) {
  rdata <- laps_data %>%
    group_by(DriverNumber) %>%
    arrange(Time) %>%
    mutate(Stint_Actual = 1 + cumsum(!is.na(PitOutTime))) %>%
    group_by(DriverNumber, Stint_Actual) %>%
    arrange(LapStartTime) %>%
    mutate(StintLapNumber = row_number()) %>%
    ungroup()
}

write_new_data <- function() {
  base_url <- "http://127.0.0.1:5000"
  page_events <- "fast_f1_events_all"
  page_sessions <- "fast_f1_sessions"
  page_weather <- "fast_f1_weather"
  page_laps <- "fast_f1_laps"
  page_car <- "fast_f1_car_data"
  page_timezones <- "fast_f1_timezones"
  year <- "2023"
  event <- "Bahrain Grand Prix"
  session <- "Q"
  # year <- "2023"
  # event <- "Pre-Season Testing"
  # session <- "2"
  
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
  
  # url_sessions <- URLencode(paste0(base_url, "/", page_sessions, "/", year, "/", event))
  # sessions <- fromJSON(url_sessions)
  # sessions <- sessions %>%
  #   left_join(timezones, by = c("Country", "Location")) %>%
  #   rowwise() %>%
  #   mutate(SessionDate = ms_to_datetime(SessionDate, Timezone)) %>%
  #   ungroup
  
  url_weather <- URLencode(paste0(base_url, "/", page_weather, "/", year, "/", event, "/", session))
  weather <- fromJSON(url_weather)
  
  url_laps <- URLencode(paste0(base_url, "/", page_laps, "/", year, "/", event, "/", session))
  #url_laps <- URLencode(paste0(base_url, "/", page_laps, "/2022/Bahrain Grand Prix/FP1"))
  laps <- fromJSON(url_laps)
  
  url_car <- URLencode(paste0(base_url, "/", page_car, "/", year, "/", event, "/", session))
  car_data <- fromJSON(url_car)
  
  write_csv(weather, "2023_weather.csv")
  write_csv(laps, "2023_laps.csv")
  write_csv(car_data, "2023_car_data.csv")
}


#write_new_data()

weather <- read_csv("2023_weather.csv")
laps <- add_my_stint(read_csv("2023_laps.csv")) %>%
  mutate(Time = ms_to_duration(Time),
         LapTime = ms_to_duration(LapTime),
         PitOutTime = ms_to_duration(PitOutTime),
         PitInTime = ms_to_duration(PitInTime),
         Sector1Time = ms_to_duration(Sector1Time),
         Sector2Time = ms_to_duration(Sector2Time),
         Sector3Time = ms_to_duration(Sector3Time),
         Sector1SessionTime = ms_to_duration(Sector1SessionTime),
         Sector2SessionTime = ms_to_duration(Sector2SessionTime),
         Sector3SessionTime = ms_to_duration(Sector3SessionTime),
         LapStartTime = ms_to_duration(LapStartTime),
         LapStartDate = ms_to_datetime(LapStartDate))
car_data <- read_csv("2023_car_data.csv") %>%
  mutate(Brake = as.numeric(Brake) * 100,
         SessionTime = ms_to_duration(SessionTime))

laps_for_driver_stint <- function(laps_data, car_num, stint_num) {
  rdf <- laps %>%
    filter(DriverNumber == car_num,
           Stint_Actual == stint_num)
}
# Give a set of laps data and all car data. This function will combine the two only selecting
# car data from the car/lap combinations in the given laps.
car_telemetry_for_lap_data <- function(laps_data, car_data) {
  
  tel_df <- laps_data %>%
    left_join(car_data, by = c("DriverNumber" = "CarNumber")) %>%
    filter(SessionTime >= LapStartTime,
           SessionTime < Sector3SessionTime)
}

# Car stint info ----
car_stint <- laps_for_driver_stint(laps, 10, 2)

p <- ggplot(data = car_stint, aes(x = StintLapNumber, y = LapTime)) +
  geom_line() +
  geom_point(aes(color = IsAccurate))
ggplotly(p)


all_stints <- laps %>% filter(DriverNumber == 10) #%>%
  # filter(is.na(PitInTime),
  #        is.na(PitOutTime))

p <- ggplot(data = all_stints, aes(x = LapStartTime, y = LapTime)) +
  geom_line(aes(color = as.character(Stint_Actual))) +
  scale_color_discrete(name = "Stint", breaks = sort(as.numeric(rownames(all_stints))))
#+
#  geom_point(aes(color = IsAccurate))
ggplotly(p)

# Lap analysis ----
driver_laps <- laps %>%
  filter(DriverNumber %in% c(27, 10, 44, 4))
telemetry_df <- car_telemetry_for_lap_data(driver_laps, car_data)
agg_telemetry_df <- telemetry_df %>%
  group_by(DriverNumber, LapNumber, LapTime) %>%
  mutate(Sector = ifelse(is.na(Sector1SessionTime) | SessionTime < Sector1SessionTime,
                         1,
                         ifelse(SessionTime < Sector2SessionTime,
                                2,
                                3))) %>%
  group_by(DriverNumber, LapNumber, LapTime, Compound, TyreLife, Team, Sector) %>%
  summarise(PitInLap = any(!is.na(PitInTime)),
            PitOutLap = any(!is.na(PitOutTime)),
            SectorMeanThrottle = mean(Throttle),
            SectorMeanThrottleMax = sum(Throttle >= 95) / n(),
            SectorMeanBrake = mean(Brake),
            SectorMeanBrakeMax = sum(Brake >= 95) / n(),
            SectorMeanT2B = ifelse(SectorMeanBrake <= 0,
                                   Inf, 
                                   SectorMeanThrottle / SectorMeanBrake),
            SectorPropLift = sum(Throttle < 5 & Brake < 5) / n(),
            SectorMeanRPM = mean(RPM),
            SectorMeanGear = mean(nGear),
            SectorWeight = n(), .groups = "drop") %>%
  mutate(PosPush = !PitInLap & !PitOutLap) %>%
  distinct()

pairs_df <- agg_telemetry_df %>%
  select(-c(Team, Compound))

pairs(pairs_df)

ggplot(agg_telemetry_df, aes(SectorMeanT2B, LapTime, color = PosPush, shape = (as.factor(Sector)))) +
  geom_point()
ggplot(agg_telemetry_df, aes(SectorMeanRPM, LapTime, color = PosPush, shape = (as.factor(Sector)))) +
  geom_point()

