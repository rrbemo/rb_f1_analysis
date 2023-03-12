#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Helper Functions ----

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

car_telemetry_for_lap_data <- function(laps_data, car_data) {
    
    tel_df <- laps_data %>%
        left_join(car_data, by = c("DriverNumber" = "CarNumber")) %>%
        filter(SessionTime >= LapStartTime,
               SessionTime < Sector3SessionTime)
}

# Data pulling from files ----
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

# Shiny Server ----
shinyServer(function(input, output) {

    # Reactive data ----
    driver_laps <- reactive({
        req(input$driver_num)
        
        driver_num <- input$driver_num
        laps %>%
            filter(DriverNumber %in% driver_num)
    })
    
    telemetry_df <- reactive({
        req(driver_laps)
        car_telemetry_for_lap_data(driver_laps(), car_data)
    })
    
    agg_telemetry_df <- reactive({
        req(telemetry_df)
        
        telemetry_df() %>%
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
            group_by(DriverNumber, LapNumber) %>%
            mutate(LapMeanThrottle = weighted.mean(SectorMeanThrottle, SectorWeight),
                   LapMeanThrottleMax = weighted.mean(SectorMeanThrottleMax, SectorWeight),
                   LapMeanBrake = weighted.mean(SectorMeanBrake, SectorWeight),
                   LapMeanBrakeMax = weighted.mean(SectorMeanBrakeMax, SectorWeight),
                   LapMeanT2B = weighted.mean(SectorMeanT2B, SectorWeight),
                   LapPropLift = weighted.mean(SectorPropLift, SectorWeight),
                   LapMeanRPM = weighted.mean(SectorMeanRPM, SectorWeight),
                   LapMeanGear = weighted.mean(SectorMeanGear, SectorWeight), .groups = "drop") %>%
            mutate(PosPush = !PitInLap & !PitOutLap) %>%
            distinct()
    })
    
    # UI outputs ----
    output$ui_driver_options <- renderUI({
        print(unique(laps$DriverNumber))
        ui <- pickerInput("driver_num",
                          label = "Driver number",
                          multiple = TRUE,
                          selected = c(27, 10, 44, 4), #c("27", "10", "44", "4"),
                          choices = unique(laps$DriverNumber))
    })
    
    # Plot outputs ----
    output$analysis_plot1 <- renderPlotly({
        req(agg_telemetry_df)
        
        ggp <- ggplot(agg_telemetry_df(), aes(SectorMeanT2B, LapTime, color = PosPush, shape = (as.factor(Sector)))) +
            geom_point()
        ggplotly(ggp)
    })
    output$analysis_plot2 <- renderPlotly({
        req(agg_telemetry_df)
        
        ggp <- ggplot(agg_telemetry_df(), aes(SectorMeanRPM, LapTime, color = PosPush, shape = (as.factor(Sector)))) +
            geom_point()
        ggplotly(ggp)
    })
    output$analysis_plot3 <- renderPlotly({
        req(agg_telemetry_df)
        
        ggp <- ggplot(agg_telemetry_df(), aes(LapMeanT2B, LapTime, color = Team, shape = PosPush)) +
            geom_point()
        ggplotly(ggp)
    })
    output$analysis_plot4 <- renderPlotly({
        req(agg_telemetry_df)
        
        ggp <- ggplot(agg_telemetry_df(), aes(LapMeanRPM, LapTime, color = Team, shape = PosPush)) +
            geom_point()
        ggplotly(ggp)
    })

})
