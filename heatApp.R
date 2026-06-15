#======= PHASE 5: H.E.A.T / DASHBOARD ==========

library(shiny) #app framework, user interaction
library(shinydashboard) #dashboard (header, sidebar, boxes,body, etc)
library(dplyr) #data manipulation (filter, group)
library(ggplot2) #static charts
library(lubridate) #date and time
library(plotly) #dynamic charts
library(DT) #interactive tables (sort rows)
library(zoo) #rollapply (7-day rolling avg)

#load clean data
clean_data <- read.csv("clean_data.csv")
clean_data$date <- as.Date(clean_data$date)
clean_data$datetime <- as.POSIXct(clean_data$datetime)

if(!"hour" %in% names(clean_data)) {
  clean_data$hour <- hour(clean_data$datetime)
}

#load models made from phase 4
if(file.exists("linear_regression_model.rds")) {
  lr_model <- readRDS("linear_regression_model.rds")
}

if(file.exists("classification_model.rds")) {
  rf_model <- readRDS("classification_model.rds")
}

#mon-sun week function
get_current_week <- function() {
  today <- max(clean_data$date)
  current_monday <- floor_date(today, unit = "week", week_start = 1)
  current_sunday <- current_monday + 6
  return(list(start = current_monday, end = current_sunday))
}

#h.i. category function
get_category <- function(heat_index) {
  if(is.na(heat_index)) return("No Data")
  if(heat_index < 27) return("No Caution")
  if(heat_index >= 27 & heat_index < 33) return("Caution")
  if(heat_index >= 33 & heat_index < 42) return("Extreme Caution")
  if(heat_index >= 42 & heat_index < 52) return("Danger")
  if(heat_index >= 52) return("Extreme Danger")
  return("Unknown")
}

#more readable hour function
format_hour <- function(hour) {
  if(is.na(hour)) return("N/A")
  if(hour == 0) return("12 AM")
  if(hour == 12) return("12 PM")
  if(hour < 12) return(paste0(hour, " AM"))
  return(paste0(hour - 12, " PM"))
}

#==========================UI=================================

ui <- dashboardPage(
  
  #header
  dashboardHeader(
    title = tags$div(
      style = "font-family: 'Georgia', sans-serif; font-weight: bold; font-size: 20px;",
      "H.E.A.T.: Heat Index Tracker"
    ),
    titleWidth = 350
  ),
  
  #sidebar
  dashboardSidebar(
    width = 280,
    
    #sidebar menu
    sidebarMenu(
      id = "tabs",
      menuItem("Daily Statistics", tabName = "daily", icon = icon("calendar-day")),
      menuItem("Weekly Statistics", tabName = "weekly", icon = icon("calendar-week")),
      menuItem("Highest Heat Index", tabName = "q1", icon = icon("thermometer-full")),
      menuItem("Heat Index Range", tabName = "q2", icon = icon("arrows-alt-h")),
      menuItem("Peak Heat Hour", tabName = "q3", icon = icon("clock")),
      menuItem("Weekly Average", tabName = "q4", icon = icon("chart-line")),
      menuItem("Geographic Comparison", tabName = "q5", icon = icon("map-marker-alt")),
      menuItem("Heat Risk Category Forecast", tabName = "risk", icon = icon("exclamation-triangle"))
    ),
    
    hr(),
    
    #city selector
    selectInput("city", "Select City:", 
                choices = unique(clean_data$city), 
                selected = "Quezon City"),
    
    #date picker for daily stats
    dateInput("selected_date", "Select Date for Daily Statistics:",
              value = Sys.Date(),
              min = min(clean_data$date),
              max = max(clean_data$date)),
    
    hr(),
    
    #prediction calculator
    box(
      title = "Heat Index Prediction Calculator", 
      status = "danger", 
      solidHeader = TRUE,
      width = 12,
      
      #temp
      div(
        style = "margin-bottom: 15px;",
        tags$label(" Temperature", style = "font-weight: bold; color: #555; display: block; margin-bottom: 5px;"),
        tags$small("Enter temperature in degrees Celsius (°C)", style = "color: #888; font-size: 11px;"),
        numericInput("temp_input", NULL, value = 32, step = 0.5)
      ),
      
      #humidity
      div(
        style = "margin-bottom: 15px;",
        tags$label("Humidity", style = "font-weight: bold; color: #555; display: block; margin-bottom: 5px;"),
        tags$small("Enter relative humidity (%)", style = "color: #888; font-size: 11px;"),
        numericInput("humidity_input", NULL, value = 70, step = 1)
      ),
      
      #predict button
      div(
        style = "text-align: center; margin: 20px 0 15px 0;",
        actionButton("predict_btn", "Predict Heat Index", class = "btn-primary")
      ),
      
      br(),
      
      #prediction result
      div(
        style = "background-color: #FFF3E0; padding: 12px; border-radius: 8px; border-left: 4px solid #a83327;",
        h5("Prediction Result:", style = "color: #a83327; font-weight: bold; margin-top: 0; margin-bottom: 8px; text-align: center;"),
        div(
          style = "text-align: center;",
          verbatimTextOutput("prediction_result")
        )
      )
    )
  ),
  
  #body
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f9f9f9;
        }
        .skin-blue .main-header .logo {
          background-color: #a83327;
        }
        .skin-blue .main-header .navbar {
          background-color: #a83327;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #8a1f15;
        }
        .small-box {
          border-radius: 10px;
        }
        
            #predict_btn {
          background-color: #a83327 !important;
          color: white !important;
          border: none !important;
          width: auto !important;
          min-width: 180px !important;
          padding: 8px 25px !important;
          font-weight: bold !important;
          border-radius: 5px !important;
          transition: all 0.3s ease !important;
        }
        
        #predict_btn:hover {
          background-color: #7a1f15 !important;
          cursor: pointer !important;
        }
        
        /* Hide default numericInput labels since we added custom ones */
        .form-group.shiny-input-container > label {
          display: none !important;
        }
        
        #prediction_result {
          text-align: center !important;
          background-color: transparent !important;
          padding: 5px !important;
          color: #333 !important;
          font-weight: 500 !important;
        }
      "))
    ),
    
    #tab items
    tabItems(
      
      #daily stats++++++++++++++++++++++++++++++++++++++++
      tabItem(
        tabName = "daily",
        h2("Daily Heat Index Statistics", style = "color: #a83327;"),
        hr(),
        fluidRow(
          valueBoxOutput("daily_highest_box", width = 3),
          valueBoxOutput("daily_current_box", width = 3),
          valueBoxOutput("daily_peak_hour_box", width = 3),
          valueBoxOutput("daily_avg_box", width = 3)
        ),
        fluidRow(
          valueBoxOutput("daily_range_box", width = 3),
          valueBoxOutput("daily_category_box", width = 3)
        ),
        fluidRow(
          box(
            title = "Hourly Heat Index", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("daily_hourly_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Hourly Data Table", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            DTOutput("daily_table")
          )
        )
      ),
      
      #weekly stats+++++++++++++++++++++++++++++++++++++
      tabItem(
        tabName = "weekly",
        h2(textOutput("weekly_title"), style = "color: #a83327;"),
        hr(),
        fluidRow(
          valueBoxOutput("weekly_highest_box", width = 3),
          valueBoxOutput("weekly_avg_box", width = 3),
          valueBoxOutput("weekly_range_box", width = 3),
          valueBoxOutput("weekly_peak_hour_box", width = 3)
        ),
        fluidRow(
          valueBoxOutput("weekly_category_box", width = 3),
          valueBoxOutput("weekly_rolling_avg_box", width = 3)
        ),
        fluidRow(
          box(
            title = "7-Day Rolling Average Trend", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("weekly_rolling_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Daily Breakdown (Mon-Sun)", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            DTOutput("weekly_table")
          )
        )
      ),
      
      #Q1 TAB+++++++++++++++++++++++++++++++
      tabItem(
        tabName = "q1",
        h2("Highest Heat Index", style = "color: #a83327;"),
        p("What is the highest heat index temperature within a 7-day period?"),
        fluidRow(
          box(
            title = "Top 10 Cities", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("q1_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "All Cities - Complete List", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            DTOutput("q1_table")
          )
        )
      ),
      
      #Q2 TAB+++++++++++++++++++++++++++++++
      tabItem(
        tabName = "q2",
        h2("Heat Index Range by City", style = "color: #a83327;"),
        p("What is the range of heat index variation from lowest to highest within a 7-day period?"),
        fluidRow(
          box(
            title = "Top 15 Cities", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("q2_plot", height = "600px")
          )
        ),
        fluidRow(
          box(
            title = "All Cities - Complete List", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            DTOutput("q2_table")
          )
        )
      ),
      
      #Q3 TAB+++++++++++++++++++++++++++++++
      tabItem(
        tabName = "q3",
        h2("Peak Heat Hour Distribution", style = "color: #a83327;"),
        p("What period of the day records the highest heat index within a 7-day period?"),
        fluidRow(
          box(
            title = "Peak Hour Distribution", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("q3_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Data Table", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            DTOutput("q3_table")
          )
        )
      ),
      
      #Q4 TAB+++++++++++++++++++++++++++++++
      tabItem(
        tabName = "q4",
        h2("Weekly Average Heat Index", style = "color: #a83327;"),
        p("What is the average heat index within a 7-day period?"),
        fluidRow(
          box(
            title = "Top 15 Cities", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("q4_plot", height = "600px")
          )
        ),
        fluidRow(
          box(
            title = "All Cities - Complete List", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            DTOutput("q4_table")
          )
        )
      ),
      
      #Q5 TAB+++++++++++++++++++++++++++++++
      tabItem(
        tabName = "q5",
        h2("Heat Index Summary by Province", style = "color: #a83327;"),
        p("How do heat index patterns compare across different provinces?"),
        fluidRow(
          box(
            title = "Province Comparison", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("q5_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Data Table", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            DTOutput("q5_table")
          )
        )
      ),
      
      #HEAT RISK CLASSIFICATION++++++++++++++++++++++++
      tabItem(
        tabName = "risk",
        h2("Heat Risk Category Forecast by City", style = "color: #a83327;"),
        p("Random Forest classification predicting PAGASA heat danger categories"),
        
        #DAILY SECTION-----------------------
        h3("Daily Risk Forecast", style = "color: #a83327; margin-top: 20px;"),
        hr(),
        fluidRow(
          valueBoxOutput("risk_daily_summary_box", width = 2),
          valueBoxOutput("risk_daily_nocaution_count", width = 2),
          valueBoxOutput("risk_daily_caution_count", width = 2),
          valueBoxOutput("risk_daily_extreme_caution_count", width = 2),
          valueBoxOutput("risk_daily_danger_count", width = 2),
          valueBoxOutput("risk_daily_extreme_danger_count", width = 2)
        ),
        fluidRow(
          box(
            title = "Daily Risk Distribution", 
            status = "danger", 
            solidHeader = TRUE,
            width = 5,
            plotlyOutput("risk_daily_plot", height = "450px")
          ),
          box(
            title = "Daily Risk Table (Latest Day Average per City)", 
            status = "danger", 
            solidHeader = TRUE,
            width = 7,
            DTOutput("risk_daily_table")
          )
        ),
        
        #WEEKLY---------------------------
        h3("Weekly Risk Forecast", style = "color: #a83327; margin-top: 40px;"),
        hr(),
        fluidRow(
          valueBoxOutput("risk_weekly_summary_box", width = 2),
          valueBoxOutput("risk_weekly_nocaution_count", width = 2),
          valueBoxOutput("risk_weekly_caution_count", width = 2),
          valueBoxOutput("risk_weekly_extreme_caution_count", width = 2),
          valueBoxOutput("risk_weekly_danger_count", width = 2),
          valueBoxOutput("risk_weekly_extreme_danger_count", width = 2)
        ),
        fluidRow(
          box(
            title = "Weekly Risk Distribution", 
            status = "danger", 
            solidHeader = TRUE,
            width = 5,
            plotlyOutput("risk_weekly_plot", height = "450px")
          ),
          box(
            title = "Weekly Risk Table (7-Day Average per City)", 
            status = "danger", 
            solidHeader = TRUE,
            width = 7,
            DTOutput("risk_weekly_table")
          )
        )
      )
    )
  )
)

#===================SERVER / BACKEND============================
server <- function(input, output, session) {
  
  current_week <- reactive({
    get_current_week()
  })
  
  current_week_data <- reactive({
    week <- current_week()
    clean_data %>%
      filter(date >= week$start, date <= week$end)
  })
  
  #DAILY STATS++++++++++++++++++++++++++++++++++++
  city_daily_data <- reactive({
    req(input$city, input$selected_date)
    clean_data %>%
      filter(city == input$city, date == input$selected_date)
  })
  
  output$daily_highest_box <- renderValueBox({
    data <- city_daily_data()
    if(nrow(data) == 0) {
      valueBox(value = "No Data", subtitle = "Highest Heat Index", icon = icon("thermometer"), color = "red")
    } else {
      valueBox(value = paste0(round(max(data$heat_index_c), 1), "°C"), 
               subtitle = "Highest Heat Index", icon = icon("thermometer-full"), color = "red")
    }
  })
  
  output$daily_current_box <- renderValueBox({
    data <- city_daily_data()
    if(nrow(data) == 0) {
      valueBox(value = "No Data", subtitle = "Current Heat Index", icon = icon("sun"), color = "orange")
    } else {
      current <- data %>% arrange(desc(datetime)) %>% pull(heat_index_c) %>% first()
      valueBox(value = paste0(round(current, 1), "°C"), subtitle = "Current Heat Index", 
               icon = icon("sun"), color = "orange")
    }
  })
  
  output$daily_peak_hour_box <- renderValueBox({
    data <- city_daily_data()
    if(nrow(data) == 0) {
      valueBox(value = "No Data", subtitle = "Peak Hour", icon = icon("clock"), color = "yellow")
    } else {
      peak <- data %>%
        group_by(hour) %>%
        summarise(avg_heat = mean(heat_index_c, na.rm = TRUE)) %>%
        slice_max(avg_heat, n = 1)
      valueBox(value = format_hour(peak$hour[1]), subtitle = "Peak Hour", icon = icon("clock"), color = "yellow")
    }
  })
  
  output$daily_avg_box <- renderValueBox({
    data <- city_daily_data()
    if(nrow(data) == 0) {
      valueBox(value = "No Data", subtitle = "Average Heat Index", icon = icon("chart-line"), color = "blue")
    } else {
      valueBox(value = paste0(round(mean(data$heat_index_c), 1), "°C"), 
               subtitle = "Average Heat Index", icon = icon("chart-line"), color = "blue")
    }
  })
  
  output$daily_range_box <- renderValueBox({
    data <- city_daily_data()
    if(nrow(data) == 0) {
      valueBox(value = "No Data", subtitle = "Heat Index Range", icon = icon("arrows-alt-h"), color = "purple")
    } else {
      range_val <- max(data$heat_index_c) - min(data$heat_index_c)
      valueBox(value = paste0(round(range_val, 1), "°C"), subtitle = "Heat Index Range", 
               icon = icon("arrows-alt-h"), color = "purple")
    }
  })
  
  output$daily_category_box <- renderValueBox({
    data <- city_daily_data()
    if(nrow(data) == 0) {
      valueBox(value = "No Data", subtitle = "Current Category", icon = icon("exclamation-triangle"), color = "green")
    } else {
      current <- data %>% arrange(desc(datetime)) %>% pull(heat_index_c) %>% first()
      category <- get_category(current)
      color <- switch(category,
                      "No Caution" = "green", "Caution" = "yellow",
                      "Extreme Caution" = "orange", "Danger" = "red",
                      "Extreme Danger" = "maroon", "green")
      valueBox(value = category, subtitle = "Current Category", icon = icon("exclamation-triangle"), color = color)
    }
  })
  
  output$daily_hourly_plot <- renderPlotly({
    data <- city_daily_data()
    if(nrow(data) == 0) {
      return(plot_ly() %>% layout(title = "No data available for selected date"))
    }
    p <- ggplot(data, aes(x = hour, y = heat_index_c,
                          text = paste("Hour:", hour, ":00<br>Heat Index:", round(heat_index_c, 1), "°C"))) +
      geom_line(color = "red", size = 1.2) +
      geom_point(color = "darkred", size = 2) +
      labs(title = paste("Hourly Heat Index -", input$city),
           x = "Hour of Day", y = "Heat Index (°C)") +
      theme_minimal() +
      scale_x_continuous(breaks = 0:23)
    ggplotly(p, tooltip = "text")
  })
  
  output$daily_table <- renderDT({
    data <- city_daily_data()
    if(nrow(data) == 0) {
      return(datatable(data.frame(Message = "No data available for this date")))
    }
    data %>%
      select(Hour = hour, Temperature = temperature_c, 
             Humidity = humidity_percent, `Heat Index` = heat_index_c,
             Category = heat_danger_category) %>%
      arrange(Hour) %>%
      datatable(options = list(pageLength = 24, dom = 't'))
  })
  
  #WEEKLY+++++++++++++++++++++++++++++++++++++++++++
  city_weekly_data <- reactive({
    week <- current_week()
    clean_data %>%
      filter(city == input$city, date >= week$start, date <= week$end)
  })
  
  output$weekly_title <- renderText({
    week <- current_week()
    paste("Weekly Heat Index Statistics for", input$city, "-", 
          format(week$start, "%b %d"), "to", format(week$end, "%b %d, %Y"))
  })
  
  output$weekly_highest_box <- renderValueBox({
    data <- city_weekly_data()
    if(nrow(data) == 0) {
      valueBox(value = "No Data", subtitle = "Highest (7-day)", icon = icon("thermometer"), color = "red")
    } else {
      valueBox(value = paste0(round(max(data$heat_index_c), 1), "°C"), 
               subtitle = "Highest (7-day)", icon = icon("thermometer-full"), color = "red")
    }
  })
  
  output$weekly_avg_box <- renderValueBox({
    data <- city_weekly_data()
    if(nrow(data) == 0) {
      valueBox(value = "No Data", subtitle = "Average (7-day)", icon = icon("sun"), color = "blue")
    } else {
      valueBox(value = paste0(round(mean(data$heat_index_c), 1), "°C"), 
               subtitle = "Average (7-day)", icon = icon("sun"), color = "blue")
    }
  })
  
  output$weekly_range_box <- renderValueBox({
    data <- city_weekly_data()
    if(nrow(data) == 0) {
      valueBox(value = "No Data", subtitle = "Range (7-day)", icon = icon("arrows-alt-h"), color = "purple")
    } else {
      range_val <- max(data$heat_index_c) - min(data$heat_index_c)
      valueBox(value = paste0(round(range_val, 1), "°C"), subtitle = "Range (7-day)", 
               icon = icon("arrows-alt-h"), color = "purple")
    }
  })
  
  output$weekly_peak_hour_box <- renderValueBox({
    data <- city_weekly_data()
    if(nrow(data) == 0) {
      valueBox(value = "No Data", subtitle = "Peak Hour", icon = icon("clock"), color = "yellow")
    } else {
      peak <- data %>%
        group_by(hour) %>%
        summarise(avg_heat = mean(heat_index_c)) %>%
        slice_max(avg_heat, n = 1)
      valueBox(value = format_hour(peak$hour[1]), subtitle = "Peak Hour", icon = icon("clock"), color = "yellow")
    }
  })
  
  output$weekly_category_box <- renderValueBox({
    data <- city_weekly_data()
    if(nrow(data) == 0) {
      valueBox(value = "No Data", subtitle = "Weekly Category", icon = icon("exclamation-triangle"), color = "green")
    } else {
      avg <- mean(data$heat_index_c)
      category <- get_category(avg)
      color <- switch(category,
                      "No Caution" = "green", "Caution" = "yellow",
                      "Extreme Caution" = "orange", "Danger" = "red",
                      "Extreme Danger" = "maroon", "green")
      valueBox(value = category, subtitle = "Weekly Category", icon = icon("exclamation-triangle"), color = color)
    }
  })
  
  output$weekly_rolling_avg_box <- renderValueBox({
    data <- city_weekly_data()
    if(nrow(data) < 7) {
      valueBox(value = "Need 7 days", subtitle = "7-Day Average", icon = icon("chart-line"), color = "green")
    } else {
      valueBox(value = paste0(round(mean(data$heat_index_c), 1), "°C"), 
               subtitle = "7-Day Average", icon = icon("chart-line"), color = "green")
    }
  })
  
  output$weekly_rolling_plot <- renderPlotly({
    data <- city_weekly_data()
    if(nrow(data) == 0) {
      return(plot_ly() %>% layout(title = "No data available for this week"))
    }
    
    rolling_data <- data %>%
      arrange(date) %>%
      mutate(rolling_avg = rollapply(heat_index_c, width = 7, 
                                     FUN = mean, fill = NA, 
                                     align = "right", na.rm = TRUE))
    
    p <- ggplot(rolling_data, aes(x = date, y = rolling_avg,
                                  text = paste("Date:", date, "<br>7-Day Rolling Avg:", round(rolling_avg, 1), "°C"))) +
      geom_line(color = "red", size = 1.2) +
      geom_point(color = "darkred", size = 2) +
      labs(title = paste("7-Day Rolling Average -", input$city),
           x = "Date", y = "Heat Index (°C)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$weekly_table <- renderDT({
    data <- city_weekly_data()
    if(nrow(data) == 0) {
      return(datatable(data.frame(Message = "No data available for this week"), 
                       options = list(dom = 't')))
    }
    
    data %>%
      group_by(date) %>%
      summarise(
        `Avg Heat Index` = round(mean(heat_index_c, na.rm = TRUE), 1),
        `Max Heat Index` = round(max(heat_index_c, na.rm = TRUE), 1),
        `Min Heat Index` = round(min(heat_index_c, na.rm = TRUE), 1),
        `Peak Hour` = format_hour(hour[which.max(heat_index_c)][1]),
        `Category` = get_category(mean(heat_index_c, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      arrange(date) %>%
      datatable(options = list(pageLength = 7, dom = 't'))
  })
  
  #Q1++++++++++++++++++++++++++++++++++++++++++++++++
  output$q1_plot <- renderPlotly({
    week <- current_week()
    week_data <- current_week_data()
    data <- week_data %>%
      group_by(city, province) %>%
      summarise(highest = max(heat_index_c, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(highest), city) %>%
      head(10) %>%
      mutate(city = factor(city, levels = rev(city)))
    
    p <- ggplot(data, aes(x = city, y = highest, fill = province,
                          text = paste("City:", city, "<br>Highest:", round(highest, 1), "°C"))) +
      geom_bar(stat = "identity") + 
      coord_flip() +
      labs(title = paste("Top 10 Cities - Week of", format(week$start, "%b %d"), "to", format(week$end, "%b %d")),
           x = "City", y = "Heat Index (°C)", fill = "Province") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$q1_table <- renderDT({
    week_data <- current_week_data()
    week_data %>%
      group_by(city, province) %>%
      summarise(highest = max(heat_index_c, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(highest)) %>%
      select(City = city, Province = province, `Highest (°C)` = highest) %>%
      datatable(options = list(pageLength = 50, dom = 't'))
  })
  
  #Q2++++++++++++++++++++++++++++++++++++++++++++++++
  output$q2_plot <- renderPlotly({
    week <- current_week()
    week_data <- current_week_data()
    data <- week_data %>%
      group_by(city, province) %>%
      summarise(min_heat = min(heat_index_c, na.rm = TRUE),
                max_heat = max(heat_index_c, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(range_heat = max_heat - min_heat) %>%
      arrange(desc(range_heat), city) %>%
      head(15) %>%
      mutate(city = factor(city, levels = rev(city)))
    
    p <- ggplot(data, aes(x = city, y = min_heat,
                          text = paste("City:", city, "<br>Min:", round(min_heat, 1), "°C",
                                       "<br>Max:", round(max_heat, 1), "°C",
                                       "<br>Range:", round(range_heat, 1), "°C"))) +
      geom_segment(aes(xend = city, y = min_heat, yend = max_heat), color = "purple", size = 1) +
      geom_point(aes(y = min_heat), color = "blue", size = 2) +
      geom_point(aes(y = max_heat), color = "red", size = 2) +
      coord_flip() +
      labs(title = paste("Heat Index Range - Week of", format(week$start, "%b %d"), "to", format(week$end, "%b %d")),
           x = "City", y = "Heat Index (°C)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$q2_table <- renderDT({
    week_data <- current_week_data()
    week_data %>%
      group_by(city, province) %>%
      summarise(min_heat = min(heat_index_c, na.rm = TRUE),
                max_heat = max(heat_index_c, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(range_heat = max_heat - min_heat) %>%
      arrange(desc(range_heat), city) %>% 
      select(City = city, Province = province, `Min (°C)` = min_heat, 
             `Max (°C)` = max_heat, `Range (°C)` = range_heat) %>%
      datatable(options = list(pageLength = 50, dom = 't'))
  })
  
  #Q3++++++++++++++++++++++++++++++++++++++++++++++++
  output$q3_plot <- renderPlotly({
    week <- current_week()
    week_data <- current_week_data()
    peak_by_city <- week_data %>%
      group_by(city, hour) %>%
      summarise(avg_heat = mean(heat_index_c, na.rm = TRUE), .groups = "drop") %>%
      group_by(city) %>%
      slice_max(avg_heat, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    data <- peak_by_city %>%
      count(hour) %>%
      arrange(hour)
    data$time_label <- sapply(data$hour, format_hour)
    
    p <- ggplot(data, aes(x = reorder(time_label, hour), y = n,
                          text = paste("Time:", time_label, "<br>Cities:", n))) +
      geom_bar(stat = "identity", fill = "darkorange", alpha = 0.8) +
      geom_text(aes(label = n), vjust = -0.5, size = 4, fontface = "bold") +
      labs(title = paste("Peak Heat Hour - Week of", format(week$start, "%b %d"), "to", format(week$end, "%b %d")),
           x = "Time of Day", y = "Number of Cities") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$q3_table <- renderDT({
    week_data <- current_week_data()
    peak_by_city <- week_data %>%
      group_by(city, hour) %>%
      summarise(avg_heat = mean(heat_index_c, na.rm = TRUE), .groups = "drop") %>%
      group_by(city) %>%
      slice_max(avg_heat, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    data <- peak_by_city %>%
      count(hour) %>%
      arrange(hour)
    data$time_label <- sapply(data$hour, format_hour)
    
    data %>%
      select(`Time of Day` = time_label, `Hour` = hour, `Number of Cities` = n) %>%
      arrange(desc(`Number of Cities`)) %>%
      datatable(options = list(pageLength = 10, dom = 't'))
  })
  
  #Q4++++++++++++++++++++++++++++++++++++++++++++++++
  output$q4_plot <- renderPlotly({
    week <- current_week()
    week_data <- current_week_data()
    data <- week_data %>%
      group_by(city, province) %>%
      summarise(weekly_avg = mean(heat_index_c, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(weekly_avg)) %>%
      head(15)
    
    p <- ggplot(data, aes(x = reorder(city, weekly_avg), y = weekly_avg, fill = province,
                          text = paste("City:", city, "<br>Weekly Avg:", round(weekly_avg, 1), "°C"))) +
      geom_bar(stat = "identity") + coord_flip() +
      labs(title = paste("Weekly Average - Week of", format(week$start, "%b %d"), "to", format(week$end, "%b %d")),
           x = "City", y = "Heat Index (°C)", fill = "Province") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$q4_table <- renderDT({
    week_data <- current_week_data()
    week_data %>%
      group_by(city, province) %>%
      summarise(weekly_avg = mean(heat_index_c, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(weekly_avg)) %>%
      select(City = city, Province = province, `Weekly Avg (°C)` = weekly_avg) %>%
      datatable(options = list(pageLength = 50, dom = 't'))
  })
  
  #Q5++++++++++++++++++++++++++++++++++++++++++++++++
  output$q5_plot <- renderPlotly({
    data <- clean_data %>%
      group_by(province, region) %>%
      summarise(min_heat = min(heat_index_c, na.rm = TRUE),
                avg_heat = mean(heat_index_c, na.rm = TRUE),
                max_heat = max(heat_index_c, na.rm = TRUE),
                .groups = "drop") %>%
      tidyr::pivot_longer(cols = c(min_heat, avg_heat, max_heat),
                          names_to = "measure", values_to = "temperature") %>%
      mutate(measure = factor(measure, 
                              levels = c("min_heat", "avg_heat", "max_heat"),
                              labels = c("Minimum", "Average", "Maximum")))
    
    p <- ggplot(data, aes(x = reorder(province, -temperature), y = temperature, fill = measure,
                          text = paste("Province:", province, "<br>Measure:", measure,
                                       "<br>Heat Index:", round(temperature, 1), "°C"))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) + coord_flip() +
      labs(title = "Heat Index Summary by Province (All Time)",
           x = "Province", y = "Heat Index (°C)", fill = "Measure") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$q5_table <- renderDT({
    clean_data %>%
      group_by(province, region) %>%
      summarise(min_heat = min(heat_index_c, na.rm = TRUE),
                avg_heat = mean(heat_index_c, na.rm = TRUE),
                max_heat = max(heat_index_c, na.rm = TRUE),
                .groups = "drop") %>%
      arrange(desc(avg_heat)) %>%
      select(Province = province, Region = region, `Min (°C)` = min_heat, 
             `Average (°C)` = avg_heat, `Max (°C)` = max_heat) %>%
      datatable(options = list(pageLength = 10, dom = 't'))
  })
  
  #HEAT RISK CLASSIFICATION+++++++++++++++++++++++++++++
  #DAILY----------------------------
  #get daily avg data per city (latest day)
  daily_avg_data <- reactive({
    clean_data %>%
      filter(date == max(date)) %>%
      group_by(city, province) %>%
      summarise(
        avg_temp = round(mean(temperature_c, na.rm = TRUE), 1),
        avg_humidity = round(mean(humidity_percent, na.rm = TRUE), 1),
        avg_hour = round(mean(hour, na.rm = TRUE), 0),
        .groups = "drop"
      )
  })
  
  #value boxes
  output$risk_daily_summary_box <- renderValueBox({
    data <- daily_avg_data()
    valueBox(value = paste0(nrow(data), " Cities"), 
             subtitle = "Total Cities (Daily Avg)", 
             icon = icon("city"), color = "blue")
  })
  
  output$risk_daily_nocaution_count <- renderValueBox({
    req(exists("rf_model"))
    data <- daily_avg_data()
    data$predicted <- predict(rf_model, newdata = data.frame(
      temperature_c = data$avg_temp,
      humidity_percent = data$avg_humidity,
      hour = data$avg_hour
    ))
    count <- sum(data$predicted == "No Caution")
    valueBox(value = paste0(count, " Cities"), 
             subtitle = "No Caution", 
             icon = icon("temperature-low"), color = "green")
  })
  
  output$risk_daily_caution_count <- renderValueBox({
    req(exists("rf_model"))
    data <- daily_avg_data()
    data$predicted <- predict(rf_model, newdata = data.frame(
      temperature_c = data$avg_temp,
      humidity_percent = data$avg_humidity,
      hour = data$avg_hour
    ))
    count <- sum(data$predicted == "Caution")
    valueBox(value = paste0(count, " Cities"), 
             subtitle = "Caution", 
             icon = icon("thermometer-half"), color = "yellow")
  })
  
  output$risk_daily_extreme_caution_count <- renderValueBox({
    req(exists("rf_model"))
    data <- daily_avg_data()
    data$predicted <- predict(rf_model, newdata = data.frame(
      temperature_c = data$avg_temp,
      humidity_percent = data$avg_humidity,
      hour = data$avg_hour
    ))
    count <- sum(data$predicted == "Extreme Caution")
    valueBox(value = paste0(count, " Cities"), 
             subtitle = "Extreme Caution", 
             icon = icon("thermometer-full"), color = "orange")
  })
  
  output$risk_daily_danger_count <- renderValueBox({
    req(exists("rf_model"))
    data <- daily_avg_data()
    data$predicted <- predict(rf_model, newdata = data.frame(
      temperature_c = data$avg_temp,
      humidity_percent = data$avg_humidity,
      hour = data$avg_hour
    ))
    count <- sum(data$predicted == "Danger")
    valueBox(value = paste0(count, " Cities"), 
             subtitle = "Danger", 
             icon = icon("exclamation-triangle"), color = "red")
  })
  
  output$risk_daily_extreme_danger_count <- renderValueBox({
    req(exists("rf_model"))
    data <- daily_avg_data()
    data$predicted <- predict(rf_model, newdata = data.frame(
      temperature_c = data$avg_temp,
      humidity_percent = data$avg_humidity,
      hour = data$avg_hour
    ))
    count <- sum(data$predicted == "Extreme Danger")
    valueBox(value = paste0(count, " Cities"), 
             subtitle = "Extreme Danger", 
             icon = icon("skull-crossbones"), color = "maroon")
  })
  
  #table
  output$risk_daily_table <- renderDT({
    if(!exists("rf_model")) {
      return(datatable(data.frame(Message = "Classification model not loaded. Run Phase 4 first.")))
    }
    
    data <- daily_avg_data()
    
    #using rf_model from phase4
    data$predicted_category <- predict(rf_model, newdata = data.frame(
      temperature_c = data$avg_temp,
      humidity_percent = data$avg_humidity,
      hour = data$avg_hour
    ))
    
    #calculate heat index for comparison (threshold column)
    data$calculated_heat_index <- mapply(
      function(temp, hum) {
        result <- heat.index(t = temp, rh = hum, 
                             temperature.metric = "celsius", 
                             output.metric = "celsius")
        return(round(result, 1))
      },
      data$avg_temp, 
      data$avg_humidity
    )
    
    data$threshold_category <- sapply(data$calculated_heat_index, get_category)
    
    #sort by risk categroy (xtreme danger first)
    data <- data %>%
      mutate(risk_order = case_when(
        predicted_category == "Extreme Danger" ~ 1,
        predicted_category == "Danger" ~ 2,
        predicted_category == "Extreme Caution" ~ 3,
        predicted_category == "Caution" ~ 4,
        TRUE ~ 5
      )) %>%
      arrange(risk_order, desc(calculated_heat_index)) %>%
      select(-risk_order)
    
    datatable(
      data %>%
        select(City = city, Province = province,
               `Temp (°C)` = avg_temp,
               `Humidity (%)` = avg_humidity,
               `Heat Index` = calculated_heat_index,
               `ML Predicted` = predicted_category,
               `Threshold` = threshold_category),
      options = list(
        pageLength = 10, 
        dom = 'ftp', 
        ordering = TRUE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(width = '90px', targets = 0),
          list(width = '80px', targets = 1),
          list(width = '45px', targets = 2),
          list(width = '45px', targets = 3),
          list(width = '45px', targets = 4),
          list(width = '90px', targets = 5),
          list(width = '90px', targets = 6)
        )
      ),
      rownames = FALSE,
    ) %>%
      formatStyle("ML Predicted",
                  backgroundColor = styleEqual(
                    c("Extreme Danger", "Danger", "Extreme Caution", "Caution", "No Caution"),
                    c("#ff6666", "#ffcccc", "#ffe0b3", "#ffffcc", "#ccffcc")
                  )
      )
  })
  
  
  #plot
  output$risk_daily_plot <- renderPlotly({
    if(!exists("rf_model")) return(NULL)
    
    data <- daily_avg_data()
    data$predicted <- predict(rf_model, newdata = data.frame(
      temperature_c = data$avg_temp,
      humidity_percent = data$avg_humidity,
      hour = data$avg_hour
    ))
    
    plot_data <- data %>%
      group_by(predicted) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(predicted = factor(predicted, 
                                levels = c("Extreme Danger", "Danger", "Extreme Caution", "Caution", "No Caution")))
    
    colors <- c("Extreme Danger" = "#8B0000", "Danger" = "#ff4444",
                "Extreme Caution" = "#ff9933", "Caution" = "#ffcc00",
                "No Caution" = "#66cc66")
    
    plot_ly(plot_data, x = ~predicted, y = ~count, type = "bar",
            marker = list(color = colors[as.character(plot_data$predicted)]),
            text = ~count, textposition = "auto",
            hovertemplate = "<b>%{x}</b><br>Cities: %{y}<extra></extra>") %>%
      layout(title = "Daily Risk Distribution (Latest Day Average)",
             xaxis = list(title = "PAGASA Category"),
             yaxis = list(title = "Number of Cities", dtick = 1))
  })
  
  #WEEKLY------------------
  #get weekly avg per city
  weekly_avg_data <- reactive({
    clean_data %>%
      group_by(city, province) %>%
      summarise(
        weekly_avg_temp = round(mean(temperature_c, na.rm = TRUE), 1),
        weekly_avg_humidity = round(mean(humidity_percent, na.rm = TRUE), 1),
        weekly_avg_hour = round(mean(hour, na.rm = TRUE), 0),
        .groups = "drop"
      )
  })
  
  #value boxes
  output$risk_weekly_summary_box <- renderValueBox({
    data <- weekly_avg_data()
    valueBox(value = paste0(nrow(data), " Cities"), 
             subtitle = "Total Cities", 
             icon = icon("city"), color = "blue")
  })
  
  output$risk_weekly_nocaution_count <- renderValueBox({
    req(exists("rf_model"))
    data <- weekly_avg_data()
    data$predicted <- predict(rf_model, newdata = data.frame(
      temperature_c = data$weekly_avg_temp,
      humidity_percent = data$weekly_avg_humidity,
      hour = data$weekly_avg_hour
    ))
    count <- sum(data$predicted == "No Caution")
    valueBox(value = paste0(count, " Cities"), 
             subtitle = "No Caution", 
             icon = icon("temperature-low"), color = "green")
  })
  
  output$risk_weekly_caution_count <- renderValueBox({
    req(exists("rf_model"))
    data <- weekly_avg_data()
    data$predicted <- predict(rf_model, newdata = data.frame(
      temperature_c = data$weekly_avg_temp,
      humidity_percent = data$weekly_avg_humidity,
      hour = data$weekly_avg_hour
    ))
    count <- sum(data$predicted == "Caution")
    valueBox(value = paste0(count, " Cities"), 
             subtitle = "Caution", 
             icon = icon("thermometer-half"), color = "yellow")
  })
  
  output$risk_weekly_extreme_caution_count <- renderValueBox({
    req(exists("rf_model"))
    data <- weekly_avg_data()
    data$predicted <- predict(rf_model, newdata = data.frame(
      temperature_c = data$weekly_avg_temp,
      humidity_percent = data$weekly_avg_humidity,
      hour = data$weekly_avg_hour
    ))
    count <- sum(data$predicted == "Extreme Caution")
    valueBox(value = paste0(count, " Cities"), 
             subtitle = "Extreme Caution", 
             icon = icon("thermometer-full"), color = "orange")
  })
  
  output$risk_weekly_danger_count <- renderValueBox({
    req(exists("rf_model"))
    data <- weekly_avg_data()
    data$predicted <- predict(rf_model, newdata = data.frame(
      temperature_c = data$weekly_avg_temp,
      humidity_percent = data$weekly_avg_humidity,
      hour = data$weekly_avg_hour
    ))
    count <- sum(data$predicted == "Danger")
    valueBox(value = paste0(count, " Cities"), 
             subtitle = "Danger", 
             icon = icon("exclamation-triangle"), color = "red")
  })
  
  output$risk_weekly_extreme_danger_count <- renderValueBox({
    req(exists("rf_model"))
    data <- weekly_avg_data()
    data$predicted <- predict(rf_model, newdata = data.frame(
      temperature_c = data$weekly_avg_temp,
      humidity_percent = data$weekly_avg_humidity,
      hour = data$weekly_avg_hour
    ))
    count <- sum(data$predicted == "Extreme Danger")
    valueBox(value = paste0(count, " Cities"), 
             subtitle = "Extreme Danger", 
             icon = icon("skull-crossbones"), color = "maroon")
  })
  
  #table
  output$risk_weekly_table <- renderDT({
    if(!exists("rf_model")) {
      return(datatable(data.frame(Message = "Classification model not loaded. Run Phase 4 first.")))
    }
    
    data <- weekly_avg_data()
    
    #use rf_model from phase 4
    data$predicted_category <- predict(rf_model, newdata = data.frame(
      temperature_c = data$weekly_avg_temp,
      humidity_percent = data$weekly_avg_humidity,
      hour = data$weekly_avg_hour
    ))
    
    #calculate heat index for comparison
    data$calculated_heat_index <- mapply(
      function(temp, hum) {
        result <- heat.index(t = temp, rh = hum, 
                             temperature.metric = "celsius", 
                             output.metric = "celsius")
        return(round(result, 1))
      },
      data$weekly_avg_temp, 
      data$weekly_avg_humidity
    )
    
    data$threshold_category <- sapply(data$calculated_heat_index, get_category)
    
    #sort by risk categroy (xtreme danger first)
    data <- data %>%
      mutate(risk_order = case_when(
        predicted_category == "Extreme Danger" ~ 1,
        predicted_category == "Danger" ~ 2,
        predicted_category == "Extreme Caution" ~ 3,
        predicted_category == "Caution" ~ 4,
        TRUE ~ 5
      )) %>%
      arrange(risk_order, desc(calculated_heat_index)) %>%
      select(-risk_order)
    
    datatable(
      data %>%
        select(City = city, Province = province,
               `Temp (°C)` = weekly_avg_temp,
               `Humidity (%)` = weekly_avg_humidity,
               `Heat Index` = calculated_heat_index,
               `ML Predicted` = predicted_category,
               `Threshold` = threshold_category),
      options = list(
        pageLength = 10, 
        dom = 'ftp', 
        ordering = TRUE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(width = '90px', targets = 0),
          list(width = '80px', targets = 1),
          list(width = '45px', targets = 2),
          list(width = '45px', targets = 3),
          list(width = '45px', targets = 4),
          list(width = '90px', targets = 5),
          list(width = '90px', targets = 6)
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle("ML Predicted",
                  backgroundColor = styleEqual(
                    c("Extreme Danger", "Danger", "Extreme Caution", "Caution", "No Caution"),
                    c("#ff6666", "#ffcccc", "#ffe0b3", "#ffffcc", "#ccffcc")
                  )
      )
  })
  
  #plot
  output$risk_weekly_plot <- renderPlotly({
    if(!exists("rf_model")) return(NULL)
    
    data <- weekly_avg_data()
    data$predicted <- predict(rf_model, newdata = data.frame(
      temperature_c = data$weekly_avg_temp,
      humidity_percent = data$weekly_avg_humidity,
      hour = data$weekly_avg_hour
    ))
    
    plot_data <- data %>%
      group_by(predicted) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(predicted = factor(predicted, 
                                levels = c("Extreme Danger", "Danger", "Extreme Caution", "Caution", "No Caution")))
    
    colors <- c("Extreme Danger" = "#8B0000", "Danger" = "#ff4444",
                "Extreme Caution" = "#ff9933", "Caution" = "#ffcc00",
                "No Caution" = "#66cc66")
    
    plot_ly(plot_data, x = ~predicted, y = ~count, type = "bar",
            marker = list(color = colors[as.character(plot_data$predicted)]),
            text = ~count, textposition = "auto",
            hovertemplate = "<b>%{x}</b><br>Cities: %{y}<extra></extra>") %>%
      layout(title = "Weekly Risk Distribution (7-Day Average)",
             xaxis = list(title = "PAGASA Category"),
             yaxis = list(title = "Number of Cities", dtick = 1))
  })
  
  #PREDICTION++++++++++++++++++++++++++++++++++++++++++++
  observeEvent(input$predict_btn, {
    if(exists("lr_model")) {
      current_hour <- as.numeric(format(Sys.time(), "%H"))
      new_data <- data.frame(
        temperature_c = input$temp_input,
        humidity_percent = input$humidity_input,
        hour = current_hour
      )
      predicted_hi <- predict(lr_model, newdata = new_data)
      category <- get_category(predicted_hi)
      output$prediction_result <- renderText({
        paste("Predicted Heat Index:", round(predicted_hi, 1), "°C\nDanger Category:", category)
      })
    } else {
      output$prediction_result <- renderText("Model not loaded. Run Phase 4 first.")
    }
  })
}

shinyApp(ui = ui, server = server)
