library(httr)
library(jsonlite)
library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(weathermetrics)
library(zoo)
library(moments)
library(caret)
library(randomForest)

#====data frame for ncr=======

ncr_cities <- data.frame(
  city = c(
    "Caloocan", "Las Piñas", "Makati", "Malabon", "Mandaluyong",
    "Manila", "Marikina", "Muntinlupa", "Navotas", "Parañaque",
    "Pasay", "Pasig", "Quezon City", "San Juan", "Taguig", 
    "Valenzuela"
  ),
  province = rep("Metro Manila", 16),
  region = rep("NCR", 16),
  lat = c(
    14.6500, 14.4495, 14.5705, 14.6577, 14.5777,
    14.5895, 14.6331, 14.3951, 14.6580, 14.4706,
    14.5437, 14.5596, 14.6464, 14.6052, 14.5289,
    14.6917
  ),
  lon = c(
    120.9906, 120.9826, 121.0273, 120.9509, 121.0337,
    120.9816, 121.0992, 121.0442, 120.9478, 121.0223,
    121.9949, 121.0812, 121.0501, 121.0294, 121.0699,
    120.9695
  ),
  stringsAsFactors = FALSE
)
View(ncr_cities)

#=====data frame for calabarzon=====

calabarzon_cities <- data.frame(
  city = c(
    # Cavite (8 cities)
    "Bacoor", "Carmona", "Cavite City", "Dasmariñas", "General Trias", 
    "Imus", "Tagaytay", "Trece Martires",
    # Laguna (6 cities)
    "Biñan", "Cabuyao", "Calamba", "San Pablo", "San Pedro", "Santa Rosa",
    # Batangas (5 cities)
    "Batangas City", "Calaca", "Lipa", "Santo Tomas", "Tanauan",
    # Rizal (1 city)
    "Antipolo",
    # Quezon (2 cities)
    "Lucena", "Tayabas"
  ),
  province = c(
    # Cavite
    "Cavite", "Cavite", "Cavite", "Cavite", "Cavite",
    "Cavite", "Cavite", "Cavite",
    # Laguna
    "Laguna", "Laguna", "Laguna", "Laguna", "Laguna", "Laguna",
    # Batangas
    "Batangas", "Batangas", "Batangas", "Batangas", "Batangas",
    # Rizal
    "Rizal",
    # Quezon
    "Quezon", "Quezon"
  ),
  region = rep("CALABARZON", 22),
  lat = c(
    # Cavite
    14.4311, 14.3136, 14.4833, 14.3330, 14.3862,
    14.3897, 14.0997, 14.2813,
    # Laguna
    14.3151, 14.2716, 14.1939, 14.0745, 14.3629, 14.3147,
    # Batangas
    13.7557, 13.9313, 13.9573, 14.1107, 14.0867,
    # Rizal
    14.5862,
    # Quezon
    13.9609, 14.0156
  ),
  lon = c(
    # Cavite
    120.9679, 121.0574, 120.9088, 120.9527, 120.8809,
    120.9196, 120.9383, 120.8704,
    # Laguna
    121.0796, 121.1243, 121.1599, 121.3246, 121.0604, 121.1125,
    # Batangas
    121.0582, 120.8133, 121.1647, 121.1424, 121.1257,
    # Rizal
    121.1759,
    # Quezon
    121.6337, 121.5867
  ),
  stringsAsFactors = FALSE
)

View(calabarzon_cities)

all_cities <- bind_rows(ncr_cities, calabarzon_cities)

cat("\n===LOCATION SUMMARY====\n")
cat("NCR cities:", nrow(ncr_cities), "\n")
cat("CALABARZON cities:", nrow(calabarzon_cities), "\n")
cat("TOTAL cities:", nrow(all_cities), "\n\n")

cat("Breakdown by province:\n")
all_cities %>%
  group_by(region, province) %>%
  summarise(count = n(), .groups = "drop") %>%
  print()

View(all_cities)

#======fetch raw data=======

fetch_city_data <- function(city_name, province_name, region_name, latitude, longitude) {
  
  cat("  Fetching:", city_name, "(", province_name, ") ... ")
  
  response <- GET("https://api.open-meteo.com/v1/forecast", 
                  query = list(
                    latitude = latitude,
                    longitude = longitude,
                    hourly = "temperature_2m,relative_humidity_2m,apparent_temperature",
                    timezone = "Asia/Manila",
                    forecast_days = 7
                  ))
  
  if(status_code(response) != 200) {
    cat("FAILED\n")
    return(NULL)
  }
  
  raw_json <- content(response, as = "text")
  weather_list <- fromJSON(raw_json)
  df <- data.frame(
    city = city_name,
    province = province_name,
    region = region_name,
    datetime_raw = weather_list$hourly$time,
    temperature_c = weather_list$hourly$temperature_2m,
    humidity_percent = weather_list$hourly$relative_humidity_2m,
    apparent_temp_c = weather_list$hourly$apparent_temperature,
    fetch_time = Sys.time(),
    stringsAsFactors = FALSE
  )
  
  cat(nrow(df), "records\n")
  
  Sys.sleep(0.5)
  return(df)
}


all_data <- list()
for(i in 1:nrow(all_cities)) {
  result <- fetch_city_data(
    all_cities$city[i], 
    all_cities$province[i],
    all_cities$region[i],
    all_cities$lat[i], 
    all_cities$lon[i]
  )
  if(!is.null(result)) all_data[[i]] <- result
}

raw_data <- bind_rows(all_data)

cat("\nRAW DATA: ", nrow(raw_data), "records, ", 
    length(unique(raw_data$city)), "cities\n")
cat("  datetime_raw example:", raw_data$datetime_raw[1], "\n")

#=====save raw data=====

write.csv(raw_data, "raw_data.csv", row.names = FALSE)
cat("\nRaw data saved: raw_data.csv\n")

cat("Missing values:", sum(is.na(raw_data)), "\n")
cat("Duplicate rows:", sum(duplicated(raw_data)), "\n")

View(raw_data)
str(raw_data)

#======clean data=====

#Calculate heat index using weathermetrics package
calc_heat_index <- function(temp_c, humidity) {
  result <- heat.index(t = temp_c,
                       rh = humidity,
                       temperature.metric = "celsius",
                       output.metric = "celsius")
  return(round(result, 1))
}

#clean
clean_data <- raw_data %>%
  #Convert datetime from character to POSIXct
  mutate(
    datetime = as.POSIXct(datetime_raw, format = "%Y-%m-%dT%H:%M", tz = "Asia/Manila")
  ) %>%
  
  #Extract date components
  mutate(
    date = as_date(datetime),
    hour = hour(datetime),
    day_of_week = wday(date, label = TRUE, week_start = 1),
    week_start = floor_date(date, unit = "week", week_start = 1)
  ) %>%
  
  #Calculate Heat Index using weathermetrics package
  mutate(
    heat_index_c = mapply(calc_heat_index, temperature_c, humidity_percent)
  ) %>%
  
  #Round numeric values to 1 decimal
  mutate(
    heat_index_c = round(heat_index_c, 1),
    temperature_c = round(temperature_c, 1),
    humidity_percent = round(humidity_percent, 1)
  ) %>%
  
  #Add PAGASA Heat Danger Category
  mutate(
    heat_danger_category = case_when(
      heat_index_c >= 27 & heat_index_c <= 32 ~ "Caution",
      heat_index_c >= 33 & heat_index_c <= 41 ~ "Extreme Caution",
      heat_index_c >= 42 & heat_index_c <= 51 ~ "Danger",
      heat_index_c >= 52 ~ "Extreme Danger",
      TRUE ~ "No Caution"
    )
  )

cat("\n========== CLEANING RESULTS ==========\n")
cat("Total records:", nrow(clean_data), "\n")
cat("Missing values after cleaning:", sum(is.na(clean_data)), "\n")
cat("\nHeat danger category distribution:\n")
print(table(clean_data$heat_danger_category))

#====save clean data=========

write.csv(clean_data, "clean_data.csv", row.names = FALSE)

cat("\nCleaned data saved: clean_data.csv\n")

View(clean_data)
str(clean_data)

#=======PHASE 3: EDA======

#Q1: highest heat index(top 10 cities)++++++
#calculate Q1 results
q1_result <- clean_data %>%
  group_by(city, province) %>%
  summarise(highest = max(heat_index_c), .groups = "drop") %>%
  arrange(desc(highest))
#para makita all cities if tie
highest_value <- max(q1_result$highest)
highest_cities <- q1_result %>%
  filter(highest == highest_value) %>%
  pull(city)

#statistics
cat("\n========== Q1: HIGHEST HEAT INDEX STATISTICS ==========\n--- All Cities Ranked by Highest Heat Index ---")
print(q1_result, n = 38)

cat("\n--- Summary Statistics ---\n")
cat("Overall highest heat index across all cities:", highest_value, "°C\n")
cat("City/cities with highest heat index:", paste(highest_cities, collapse = ", "), "\n")
cat("Average highest heat index across all cities:", round(mean(q1_result$highest), 1), "°C\n")

#plot
q1plot <- q1_result %>%
  top_n(10, highest) %>%
  ggplot(aes(x = reorder(city, highest), y = highest, fill = province)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  geom_text(aes(label = paste0(round(highest, 1), "°C")), hjust = -0.2, size = 3.5) +
  labs(title = "Top 10 Cities with Highest Heat Index",
       subtitle = paste0("Highest recorded: ", paste(highest_cities, collapse = ", "), 
                         " (", highest_value, "°C) | Average: ", round(mean(q1_result$highest), 1), "°C"),
       x = "City", y = "Maximum Heat Index (°C)") +
  theme_minimal()

ggsave("chart_Q1_highest.png", q1plot, width = 9, height = 6)
q1plot


#Q2: Heat index range++++++++++
# Calculate Q2 results for ALL cities
q2_result <- clean_data %>%
  group_by(city, province) %>%
  summarise(
    min_heat = min(heat_index_c),
    max_heat = max(heat_index_c),
    range_heat = max_heat - min_heat,
    .groups = "drop"
  ) %>%
  arrange(desc(range_heat))

# Find highest range and ALL cities with that value
highest_range <- max(q2_result$range_heat)
highest_range_cities <- q2_result %>%
  filter(range_heat == highest_range) %>%
  pull(city)

# Find lowest range
lowest_range <- min(q2_result$range_heat)
lowest_range_cities <- q2_result %>%
  filter(range_heat == lowest_range) %>%
  pull(city)

# Statistics
cat("\n========== Q2: HEAT INDEX RANGE STATISTICS ==========\n")
cat("--- All Cities - Min, Max, and Range (", nrow(q2_result), "cities) ---\n")
print(q2_result, n = Inf)

cat("\n--- Summary Statistics ---\n")
cat("Largest heat index range:", highest_range, "°C\n")
cat("City/cities with largest range:", paste(highest_range_cities, collapse = ", "), "\n")
cat("Smallest heat index range:", lowest_range, "°C\n")
cat("City/cities with smallest range:", paste(lowest_range_cities, collapse = ", "), "\n")
cat("Average heat index range across all cities:", round(mean(q2_result$range_heat), 1), "°C\n")

# Plot (top 10 by max_heat)
q2plot <- q2_result %>%
  top_n(38, max_heat) %>%
  ggplot(aes(x = reorder(city, max_heat), y = min_heat)) +
  
  # Line connecting min to max
  geom_segment(aes(xend = city, y = min_heat, yend = max_heat), 
               size = 1.2, color = "purple") +
  # Min dots (blue)
  geom_point(aes(y = min_heat), color = "blue", size = 3) +
  # Max dots (red)
  geom_point(aes(y = max_heat), color = "red", size = 3) +
  # Min values labels
  geom_text(aes(y = min_heat, label = paste0(round(min_heat, 1), "°C")), 
            hjust = 1.3, color = "blue", size = 3.5) +
  # Max values labels
  geom_text(aes(y = max_heat, label = paste0(round(max_heat, 1), "°C")), 
            hjust = -0.3, color = "red", size = 3) +
  
  coord_flip() +
  labs(title = "Heat Index Range by City",
       subtitle = paste0("Largest range: ", paste(highest_range_cities, collapse = ", "), 
                         " (", highest_range, "°C) | Average range: ", round(mean(q2_result$range_heat), 1), "°C"),
       x = "City", y = "Heat Index (°C)") +
  theme_minimal()

ggsave("chart_Q2_range.png", q2plot, width = 11, height = 7)
q2plot

#Q3: peak hr+++++++++++
#calculate the peak hour for each city
q3_result <- clean_data %>%
  group_by(city, hour) %>%
  summarise(avg_heat = mean(heat_index_c), .groups = "drop") %>%
  group_by(city) %>%
  slice_max(avg_heat, n = 1) %>%
  ungroup() %>%
  #readable time
  mutate(
    time_label = case_when(
      hour == 0 ~ "12 AM", hour == 1 ~ "1 AM", hour == 2 ~ "2 AM", hour == 3 ~ "3 AM",
      hour == 4 ~ "4 AM", hour == 5 ~ "5 AM", hour == 6 ~ "6 AM", hour == 7 ~ "7 AM",
      hour == 8 ~ "8 AM", hour == 9 ~ "9 AM", hour == 10 ~ "10 AM", hour == 11 ~ "11 AM",
      hour == 12 ~ "12 PM", hour == 13 ~ "1 PM", hour == 14 ~ "2 PM", hour == 15 ~ "3 PM",
      hour == 16 ~ "4 PM", hour == 17 ~ "5 PM", hour == 18 ~ "6 PM", hour == 19 ~ "7 PM",
      hour == 20 ~ "8 PM", hour == 21 ~ "9 PM", hour == 22 ~ "10 PM", hour == 23 ~ "11 PM",
      TRUE ~ paste0(hour, ":00")
    )
  )

cat("\n========== Q3: PEAK HEAT HOUR STATISTICS ==========\n--- Peak Hour by City ---\n")
q3_result %>%
  dplyr::select(city, time_label, avg_heat, hour) %>%
  arrange(hour) %>%
  print(n = 50)

#Summary of most common peak hours
peak_hour_summary <- q3_result %>%
  group_by(hour, time_label) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

top_peak <- peak_hour_summary %>% slice_max(count, n = 1)
cat("Most common peak hour:", top_peak$time_label, "\n--- Most Common Peak Hours Across All Cities ---\n")
peak_hour_summary %>%
  dplyr::select(time_label, count, hour) %>%
  print()

#plot
q3plot <- q3_result %>%
  count(hour, time_label) %>%
  ggplot(aes(x = reorder(time_label, hour), y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  # Add value labels on top of bars
  geom_text(aes(label = n), vjust = -0.5, size = 4, fontface = "bold") +
  # Gradient color (mas madami, mas red)
  scale_fill_gradient(low = "gold", high = "red") +
  labs(title = "Peak Heat Hour Distribution",
       subtitle = paste0("What time do cities experience their highest heat index? | Most common: ", 
                         top_peak$time_label, " (", top_peak$count, " cities)"),
       x = "Period of Day",
       y = "Number of Cities") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray50", size = 10),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "none",
    panel.grid.minor.x = element_blank()
  )

ggsave("chart_Q3_peak_hour.png", q3plot, width = 12, height = 6)
q3plot

#Q4: avg heat index++++++++++
#calculate rolling average
q4_result_all <- clean_data %>%
  group_by(city, province) %>%
  arrange(date) %>%
  mutate(
    rolling_avg = rollapply(heat_index_c, width = 7, 
                            FUN = mean, fill = NA, 
                            align = "right", na.rm = TRUE)
  ) %>%
  filter(date == max(date, na.rm = TRUE)) %>%  #latest date only
  distinct(city, .keep_all = TRUE) %>%
  arrange(desc(rolling_avg))

#statistics
cat("\n========== Q4: 7-DAY ROLLING AVERAGE STATISTICS ==========\n--- Latest 7-Day Rolling Average by City(", nrow(q4_result_all), "cities) ---\n")
print(q4_result_all %>% 
        dplyr::select(city, province, rolling_avg), n = Inf)

#highest and lowest rolling avg
highest_rolling <- max(q4_result_all$rolling_avg, na.rm = TRUE)
highest_rolling_cities <- q4_result_all %>%
  filter(rolling_avg == highest_rolling) %>%
  pull(city)

lowest_rolling <- min(q4_result_all$rolling_avg, na.rm = TRUE)
lowest_rolling_cities <- q4_result_all %>%
  filter(rolling_avg == lowest_rolling) %>%
  pull(city)

cat("\n--- Summary Statistics ---\n")
cat("Highest 7-day rolling average:", highest_rolling, "°C\n")
cat("City/cities with highest rolling average:", paste(highest_rolling_cities, collapse = ", "), "\n")
cat("Lowest 7-day rolling average:", lowest_rolling, "°C\n")
cat("City/cities with lowest rolling average:", paste(lowest_rolling_cities, collapse = ", "), "\n")
cat("Average rolling average across all cities:", round(mean(q4_result_all$rolling_avg, na.rm = TRUE), 1), "°C\n")

#plot
q4plot <- q4_result_all %>%
  ggplot(aes(x = reorder(city, rolling_avg), y = rolling_avg, fill = rolling_avg)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = paste0(round(rolling_avg, 1), "°C")), 
            hjust = -0.2, size = 2.5) + 
  scale_fill_gradient(low = "paleturquoise", high = "firebrick") +
  labs(title = "Latest 7-Day Rolling Average Heat Index (All Cities)",
       subtitle = paste0("Highest: ", paste(highest_rolling_cities, collapse = ", "),
                         " (", highest_rolling, "°C) | Average: ", round(mean(q4_result_all$rolling_avg, na.rm = TRUE), 1), "°C"),
       x = "City",
       y = "7-Day Rolling Average Heat Index (°C)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 7)
  )

ggsave("chart_Q4_average_7days.png", q4plot, width = 12, height = 10)
q4plot

#Q5: compare cities/areas+++++++++++++
# Calculate Q5 results for ALL provinces
q5_result <- clean_data %>%
  group_by(province, region) %>%
  summarise(
    min_heat = min(heat_index_c),
    avg_heat = mean(heat_index_c),
    max_heat = max(heat_index_c),
    sd_heat = sd(heat_index_c),  # Standard deviation for variability
    count_cities = n_distinct(city),  # Number of cities per province
    .groups = "drop"
  ) %>%
  arrange(desc(avg_heat))

# Statistics
cat("\n========== Q5: GEOGRAPHIC COMPARISON STATISTICS ==========\n--- All Provinces - Min, Avg, Max Heat Index (", nrow(q5_result), "provinces) ---\n")
print(q5_result, n = Inf)

cat("\n--- Summary Statistics by Province ---\n")
for(i in 1:nrow(q5_result)) {
  cat("\n", i, ".", q5_result$province[i], "-", q5_result$region[i], "\n")
  cat("   Cities in province:", q5_result$count_cities[i], "\n")
  cat("   Min Heat Index:", round(q5_result$min_heat[i], 1), "°C\n")
  cat("   Average Heat Index:", round(q5_result$avg_heat[i], 1), "°C\n")
  cat("   Max Heat Index:", round(q5_result$max_heat[i], 1), "°C\n")
  cat("   Range:", round(q5_result$max_heat[i] - q5_result$min_heat[i], 1), "°C\n")
}

#by regions
regional_summary <- q5_result %>%
  group_by(region) %>%
  summarise(
    avg_region_heat = mean(avg_heat),
    min_region_heat = min(min_heat),
    max_region_heat = max(max_heat),
    num_provinces = n(),
    .groups = "drop"
  )

cat("\n--- Regional Summary (NCR vs CALABARZON) ---\n")
print(regional_summary)

#hottest and coolest provinces
hottest_province <- q5_result %>% slice_max(avg_heat, n = 1)
coolest_province <- q5_result %>% slice_min(avg_heat, n = 1)

cat("\n--- Hottest and Coolest Provinces ---\n")
cat("Hottest province:", hottest_province$province, "(", hottest_province$region, ") |", 
    "Average:", round(hottest_province$avg_heat, 1), "°C |", 
    "Max:", round(hottest_province$max_heat, 1), "°C\nCoolest province:", coolest_province$province, "(", coolest_province$region, ") |", 
    "Average:", round(coolest_province$avg_heat, 1), "°C |", 
    "Max:", round(coolest_province$max_heat, 1), "°C\nTemperature difference between hottest and coolest province:", 
    round(hottest_province$avg_heat - coolest_province$avg_heat, 1), "°C\n")

#plot
q5plot <- q5_result %>%
  pivot_longer(cols = c(min_heat, avg_heat, max_heat),
               names_to = "measure",
               values_to = "temperature") %>%
  mutate(measure = factor(measure, 
                          levels = c("min_heat", "avg_heat", "max_heat"),
                          labels = c("Minimum", "Average", "Maximum"))) %>%
  ggplot(aes(x = reorder(province, -temperature), y = temperature, fill = measure)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  coord_flip() +
  geom_text(aes(label = paste0(round(temperature, 1), "°C")), 
            hjust = -0.1, size = 2.5, position = position_dodge(width = 0.8)) +
  labs(title = "Heat Index Summary by Province",
       subtitle = paste0("Hottest: ", hottest_province$province, " (", round(hottest_province$avg_heat, 1), "°C) | ", 
                         "Coolest: ", coolest_province$province, " (", round(coolest_province$avg_heat, 1), "°C) | ",
                         "Difference: ", round(hottest_province$avg_heat - coolest_province$avg_heat, 1), "°C"),
       x = "Province",
       y = "Heat Index (°C)",
       fill = "Measure") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray50", size = 9),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = c("Minimum" = "lightskyblue", 
                               "Average" = "plum", 
                               "Maximum" = "tomato"))

ggsave("chart_Q5_compare_areas.png", q5plot, width = 13, height = 9)
q5plot


#Distribution of Heat Index (overall lang for modeling)+++++++++++++++++++++

#statistics
mean_hi <- mean(clean_data$heat_index_c, na.rm = TRUE)
median_hi <- median(clean_data$heat_index_c, na.rm = TRUE)
sd_hi <- sd(clean_data$heat_index_c, na.rm = TRUE)
min_hi <- min(clean_data$heat_index_c, na.rm = TRUE)
max_hi <- max(clean_data$heat_index_c, na.rm = TRUE)
range_hi <- max_hi - min_hi

cat("\n========== HEAT INDEX DISTRIBUTION STATISTICS ==========\n--- Basic Statistics ---\nMean:", round(mean_hi, 1), 
    "°C\nMedian:", round(median_hi, 1), "°C\nStandard Deviation:", round(sd_hi, 1), 
    "°C\nMinimum:", round(min_hi, 1), "°C\nMaximum:", round(max_hi, 1), 
    "°C\nRange:", round(range_hi, 1), "°C\n")

#quartiles
q1 <- quantile(clean_data$heat_index_c, 0.25, na.rm = TRUE)
q3 <- quantile(clean_data$heat_index_c, 0.75, na.rm = TRUE)
iqr <- q3 - q1

cat("\n--- Quartiles ---\nQ1 (25th percentile):", round(q1, 1), 
    "°C\nQ2 (50th percentile / Median):", round(median_hi, 1), 
    "°C\nQ3 (75th percentile):", round(q3, 1), 
    "°C\nInterquartile Range (IQR):", round(iqr, 1), "°C\n")

#skewness (requires moments package)
skew_val <- skewness(clean_data$heat_index_c, na.rm = TRUE)
cat("\n--- Shape ---\nSkewness:", round(skew_val, 3), "\n")
if(skew_val > 0.5) {
  cat("  → Positively skewed (tail on the right / more hot days)\n")
} else if(skew_val < -0.5) {
  cat("  → Negatively skewed (tail on the left / more cool days)\n")
} else {
  cat("  → Approximately symmetric\n")
}

#most common temperature range
#bins every 5 degrees
bins <- seq(floor(min_hi), ceiling(max_hi), by = 5)
bin_counts <- clean_data %>%
  mutate(temp_bin = cut(heat_index_c, breaks = bins, include.lowest = TRUE)) %>%
  group_by(temp_bin) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

most_common_bin <- bin_counts[1, ]
most_common_range_raw <- as.character(most_common_bin$temp_bin)
#range w dash
numbers <- regmatches(most_common_range_raw, gregexpr("[0-9]+", most_common_range_raw))[[1]]
if(length(numbers) >= 2) {
  most_common_range <- paste0(numbers[1], "-", numbers[2], "°C")
} else {
  most_common_range <- paste0(numbers[1], "°C")
}

most_common_count <- most_common_bin$count
most_common_pct <- round(most_common_bin$count / nrow(clean_data) * 100, 1)

cat("\n--- Most Common Temperature Range ---\nMost common range:", most_common_range, 
    "\nRecords in this range:", most_common_count, "out of", nrow(clean_data), 
    "\nPercentage:", most_common_pct, "%\n")

#plot
plot_distribution <- clean_data %>%
  ggplot(aes(x = heat_index_c)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, 
                 fill = "gold", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1.2) +
  labs(title = "Distribution of Heat Index Values",
       subtitle = paste0("Mean: ", round(mean_hi, 1), "°C | Median: ", round(median_hi, 1),
                         "°C | SD: ", round(sd_hi, 1), "°C | Most common: ", most_common_range, " (", most_common_pct, "%)"),
       x = "Heat Index (°C)", 
       y = "Density") +
  theme_minimal()

ggsave("chart_distribution.png", plot_distribution, width = 10, height = 6)
plot_distribution

#=========PHASE 4: MODEL DEV===========
#Classification Model+++++++++++++++++++++++++
#prepare data (get available h.i. danger categories lang kasi d pwedeng walang laman sa random forest)
class_data <- clean_data %>%
  dplyr::select(temperature_c, humidity_percent, hour, heat_danger_category) %>%
  filter(!is.na(heat_danger_category)) %>%
  mutate(
    heat_danger_category = factor(heat_danger_category)  #dynamically creates levels from avail. data
  ) %>%
  na.omit()

#get available categories
available_categories <- levels(class_data$heat_danger_category)
cat("\n\n========== MODEL 1: CLASSIFICATION (Random Forest) ==========\n\n--- Available Categories in Current Data ---\n")
print(table(class_data$heat_danger_category))

#proceed if there are at least 2 categories
if(length(available_categories) >= 2) {
  
  #splits data
  set.seed(123)
  train_idx_class <- sample(1:nrow(class_data), 0.8 * nrow(class_data))
  train_class <- class_data[train_idx_class, ]
  test_class <- class_data[-train_idx_class, ]
  
  #random forest
  rf_model <- randomForest(heat_danger_category ~ temperature_c + humidity_percent + hour,
                           data = train_class, ntree = 100)
  
  cat("\n--- Model Information ---\n")
  cat("Model type: Random Forest Classification\n")
  cat("Number of categories:", length(available_categories), "\n")
  cat("Categories:", paste(available_categories, collapse = ", "), "\n")
  print(rf_model)
  
  #model eval
  test_class$predicted <- predict(rf_model, newdata = test_class)
  test_accuracy <- mean(test_class$predicted == test_class$heat_danger_category)
  
  cat("\n--- Model Evaluation ---\n")
  cat("Test Accuracy:", round(test_accuracy * 100, 1), "%\n")
  
  #confusion matrix
  conf_matrix <- table(Predicted = test_class$predicted, Actual = test_class$heat_danger_category)
  cat("\n--- Confusion Matrix ---\n")
  print(conf_matrix)
  
  #var importance
  cat("\n--- Variable Importance ---\n")
  importance_df <- data.frame(
    Variable = rownames(rf_model$importance),
    Importance = round(rf_model$importance[,1], 2)
  )
  print(importance_df[order(-importance_df$Importance), ])
  
} else {
  cat("Not enough categories for classification. Need at least 2 categories.\n")
  cat("Current unique categories:", paste(available_categories, collapse = ", "), "\n")
}

#Linear Regression Model++++++++++++++++++++++++++
#prepare data
reg_data <- clean_data %>%
  dplyr::select(temperature_c, humidity_percent, heat_index_c, hour) %>%
  na.omit()

#split
set.seed(123)
train_idx <- sample(1:nrow(reg_data), 0.8 * nrow(reg_data))
train_reg <- reg_data[train_idx, ]
test_reg <- reg_data[-train_idx, ]

#model
lm_model <- lm(heat_index_c ~ temperature_c + humidity_percent + hour, data = train_reg)

#summary
lm_summary <- summary(lm_model)

#creates coefficient table
coef_table <- data.frame(
  Variable = c("Temperature (°C)", "Humidity (%)", "Hour of Day"),
  Estimate = round(coef(lm_model)[2:4], 3),
  Std_Error = round(lm_summary$coefficients[2:4, 2], 4),
  t_value = round(lm_summary$coefficients[2:4, 3], 2),
  p_value = lm_summary$coefficients[2:4, 4],
  Significance = c("***", "***", "Not significant")
)

#format p-values
coef_table$p_value_formatted <- ifelse(coef_table$p_value < 0.001, 
                                       "< 0.001", 
                                       round(coef_table$p_value, 3))

cat("\n\n========== MODEL 2: LINEAR REGRESSION ==========\n\n   R-squared:", round(lm_summary$r.squared, 4), 
    "→ Model explains", round(lm_summary$r.squared * 100, 1), 
    "% of variation\n   Adjusted R-squared:", round(lm_summary$adj.r.squared, 4),
    "\n\nVARIABLE EFFECTS (How each factor affects Heat Index)\n" , rep("-", 50), sep = "", "\n", 
    sprintf("%-20s %12s %12s %12s %12s %15s\n", "Variable", "Estimate", "Std Error", "t-value", "p-value", "Significance"),
    paste(rep("-", 85), collapse = ""), "\n")

#print values
for(i in 1:nrow(coef_table)) {
  cat(sprintf("%-20s %12.3f %12.4f %12.2f %12s %15s\n",
              coef_table$Variable[i],
              coef_table$Estimate[i],
              coef_table$Std_Error[i],
              coef_table$t_value[i],
              coef_table$p_value_formatted[i],
              coef_table$Significance[i]))
}

#predictions frst
test_reg$predicted <- predict(lm_model, newdata = test_reg)

#calculate test R-squared
ss_res <- sum((test_reg$heat_index_c - test_reg$predicted)^2)
ss_tot <- sum((test_reg$heat_index_c - mean(test_reg$heat_index_c))^2)
test_r2 <- 1 - (ss_res / ss_tot)

cat("\n\n--- Model Evaluation ---\nTraining R-squared:", round(lm_summary$r.squared, 3), 
    "\nTest R-squared:", round(test_r2, 3), "\n\n--- Variable Interpretation ---\nTemperature coefficient:", 
    round(coef(lm_model)[2], 2), "- For every 1°C increase in temperature, heat index increases by", 
    round(abs(coef(lm_model)[2]), 1), "°C\nHumidity coefficient:", round(coef(lm_model)[3], 2), 
    "- For every 1% increase in humidity, heat index increases by", 
    round(abs(coef(lm_model)[3]), 1), "°C\nHour coefficient:", round(coef(lm_model)[4], 2), 
    "- Heat index varies by this amount per hour\n")

#plot
plot_lm <- ggplot(test_reg, aes(x = heat_index_c, y = predicted)) +
  geom_point(alpha = 0.5, color = "darkgoldenrod") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Linear Regression: Actual vs Predicted Heat Index",
       subtitle = paste0("R² = ", round(test_r2, 3), " | Points close to line = good predictions"),
       x = "Actual Heat Index (°C)", y = "Predicted Heat Index (°C)") +
  theme_minimal()

ggsave("regression_actual_vs_predicted.png", plot_lm, width = 8, height = 6)
print(plot_lm)

#============save models for dashboard==========

#classification
if(exists("rf_model")) {
  saveRDS(rf_model, "classification_model.rds")
}
#regression
saveRDS(lm_model, "linear_regression_model.rds")

cat("Files saved in current working directory:", getwd(), "\n")
