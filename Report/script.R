library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)  # For color gradient
library(ggiraphExtra)

# Load datasets
vehicle_data <- read.csv("Updated_EV_India.csv")
pollution_data <- read.csv("Pollution.csv")

# Standardize state names in vehicle_data
vehicle_data <- vehicle_data %>%
  mutate(State = case_when(
    State == "Andaman and Nicobar Islands" ~ "Andaman and Nicobar",
    State == "National Capital Territory–Delhi" ~ "Delhi",
    State == "Chhattisgarh" ~ "Chhattisgarh",
    State == "Dadra and Nagar Haveli and Daman and Diu" ~ "Daman and Diu",
    State == "Jammu and Kashmir" ~ "Jammu and Kashmir",
    State == "Ladakh" ~ "Ladakh",
    State == "Lakshadweep" ~ "Lakshadweep",
    State == "Puducherry" ~ "Puducherry",
    TRUE ~ State  # Retain other state names as they are
  ))

# Filter pollution data for Delhi
pollution_delhi <- pollution_data %>% filter(City == "Delhi")

pollution_delhi_day1 <- pollution_delhi %>% filter(Day == 1)

# Convert Month to a factor with month names
pollution_delhi_day1 <- pollution_delhi_day1 %>%
  mutate(Month_Year = paste(Month, Year, sep = "-"),
         Month = factor(Month, levels = 1:12, labels = month.abb))  # Use month abbreviations

# Calculate average AQI per year
average_aqi <- pollution_delhi_day1 %>%
  group_by(Year) %>%
  summarize(AQI_avg = mean(AQI, na.rm = TRUE))

# Plot AQI over time with Month-Year on x-axis
ggplot(pollution_delhi_day1, aes(x = Month, y = AQI, group = 1)) +
  geom_line(color = "blue") +
  geom_point(size = 2, color = "red") +
  geom_hline(data = average_aqi, aes(yintercept = AQI_avg, linetype = "Average AQI"), 
             color = "black", size = 1) +  # Dashed line for avg AQI with legend
  facet_wrap(~Year, scales = "free_x") +  # Separate plot per year
  labs(title = "AQI Over Time in Delhi (Faceted by Year)",
       x = "Month",
       y = "Air Quality Index (AQI)",
       linetype = "Legend") +  # Rename legend label
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate for readability
        axis.ticks.x = element_line(),  # Ensure x-axis ticks appear
        legend.position = "top")  # Move legend to top

forest_2021 <- read.csv('2021_forest.csv')
forest_2015 <- read.csv('2015_forest.csv')
forest_2015 <- forest_2015[-37,]
forest_2021 <- forest_2021[-37,]

# Remove "%" and convert to numeric
forest_2021 <- forest_2021 %>%
  mutate(X..of.the.forested.area.before = as.numeric(gsub("%", "", X..of.the.forested.area.before))) %>%
  rename(Total_forest_per = X..of.the.forested.area.before)

# Convert problematic columns to numeric after removing commas
forest_2015 <- forest_2015 %>%
  mutate(
    Very.dense = as.numeric(gsub(",", "", Very.dense)),  # Remove commas and convert
    Moderately.dense = as.numeric(gsub(",", "", Moderately.dense)),
    Open.forest = as.numeric(gsub(",", "", Open.forest)),
    Geographical.Area = as.numeric(gsub(",", "", Geographical.Area))
  )

forest_2015 <- forest_2015 %>%
  rename(State = `State...UT`)
forest_2021 <- forest_2021 %>%
  rename(State = `State...UT`)
vehicle_data <- vehicle_data %>%
  rename(State = `State.Name`)

# Calculate total forest percentage
forest_2015$Total_forest_per <- ((forest_2015$Very.dense + 
                                    forest_2015$Moderately.dense + 
                                    forest_2015$Open.forest) / 
                                   forest_2015$Geographical.Area) * 100

pollution_data_2015 <- pollution_data %>% filter(Year == 2015)

statewise_avg_AQI <- pollution_data %>%
  group_by(State) %>%
  summarize(Average_AQI = mean(AQI, na.rm = TRUE))

# Remove Gujarat from the dataset
merged_data <- statewise_avg_AQI %>%
  inner_join(forest_2015, by = "State") %>%
  filter(State != "Gujarat")  # Exclude Gujarat

# Remove Gujarat from the dataset
merged_data <- statewise_avg_AQI %>%
  inner_join(forest_2015, by = "State") %>%
  filter(State != "Gujarat")

# Sort states by Total Forest Cover for better visualization
merged_data <- merged_data %>%
  arrange(desc(Total_forest_per))

# Compute scaling factor for AQI and Forest Cover
scaling_factor <- max(merged_data$Total_forest_per, na.rm = TRUE) / 
  max(merged_data$Average_AQI, na.rm = TRUE)

# Add scaled AQI column **before ggplot**
merged_data <- merged_data %>%
  mutate(Scaled_AQI = Average_AQI * scaling_factor) %>%
  arrange(desc(Total_forest_per))  # Sort states by forest cover

# Dual-Axis Bar + Line Chart
ggplot(merged_data, aes(x = reorder(State, -Total_forest_per))) +
  # Bar plot for forest cover
  geom_bar(aes(y = Total_forest_per, fill = "Forest Cover (%)"), 
           stat = "identity", width = 0.6, alpha = 0.8) +
  
  # Line chart for AQI (scaled)
  geom_line(aes(y = Scaled_AQI, group = 1, color = "AQI Level"), 
            size = 1.5) + 
  
  # Points for AQI values (scaled)
  geom_point(aes(y = Scaled_AQI, color = "AQI Level"), size = 3) +
  
  # Y-Axis labels
  scale_y_continuous(
    name = "Forest Cover (%)", 
    sec.axis = sec_axis(~ . / scaling_factor, name = "Average AQI")  # Use precomputed scaling factor
  ) +
  
  # Manual color and fill adjustments
  scale_fill_manual(values = c("Forest Cover (%)" = "green")) +
  scale_color_manual(values = c("AQI Level" = "red")) +
  
  # Titles and labels
  labs(title = "Statewise Forest Cover vs AQI",
       x = "State",
       y = "Forest Cover (%)",
       fill = "Legend",
       color = "Legend") +
  
  # Theme adjustments
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

forest_change <- forest_2015 %>%
  inner_join(forest_2021, by = "State", suffix = c("_2015", "_2021")) %>%
  mutate(Change_in_forest_per = Total_forest_per_2021 - Total_forest_per_2015)

forest_change <- forest_change[,c(2,17)]

# Aggregate pollution data by state (average pollutant levels per state)
pollution_statewise <- pollution_data %>%
  group_by(State) %>%
  summarize(
    Avg_CO = mean(CO, na.rm = TRUE),
    Avg_NOx = mean(NOx, na.rm = TRUE),
    Avg_SO2 = mean(SO2, na.rm = TRUE),
    Avg_PM2.5 = mean(PM2.5, na.rm = TRUE)
  )

# Clean, filter, and convert the Non_EV_per_Area column
merged_data <- pollution_statewise %>%
  inner_join(vehicle_data, by = "State") %>%
  select(State, Non_EV_per_Area, Avg_PM2.5) %>%
  mutate(
    Non_EV_per_Area = as.numeric(gsub(",", "", Non_EV_per_Area)),
    Non_EV_per_Area = log2(Non_EV_per_Area),  # Apply log2 transformation
    State_Abbrev = abbreviate(State)
  ) %>%
  filter(Non_EV_per_Area > 0) %>%
  arrange(Non_EV_per_Area)

# Create new variables for angle and label positioning
merged_data <- merged_data %>%
  mutate(
    angle = 90 - 360 * (row_number() - 0.5) / n(),
    hjust = ifelse(angle < -90, 1, 0),
    angle = ifelse(angle < -90, angle + 180, angle)
  )

# Create the circular barplot with square root transformation
circular_barplot <- ggplot(merged_data, aes(x = reorder(State, Non_EV_per_Area), 
                                            y = Non_EV_per_Area,  
                                            fill = Avg_PM2.5)) + 
  geom_bar(stat = "identity", width = 0.6) +  # Adjust width for hollow effect
  coord_polar(start = 0) + 
  scale_fill_gradient(low = "green", high = "red", name = "PM2.5") +  # Color gradient
  labs(
    title = "Non-Electric Vehicles per Area and PM2.5 by State (Log Scale)",
    y = "Square Root of Vehicles per Area",
    x = NULL
  ) + 
  theme_minimal() + 
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.position = "right"
  ) + 
  geom_text(aes(x = State, y = max(Non_EV_per_Area) * 0.15, 
                label = State, angle = angle, hjust = hjust), 
            size = 4, color = "black")  # State names in the hollow area

# Print the circular barplot
print(circular_barplot)

# Filter the data for November 2018 for Delhi and Kerala
november_data <- pollution_data %>%
  filter(State %in% c("Delhi", "Kerala") & 
           between(as.Date(Date), as.Date("2018-10-20"), as.Date("2018-11-20")))

# Create the plot
pm25_plot <- ggplot(november_data, aes(x = as.Date(Date), y = PM2.5, color = State, group = State)) +
  # Highlight the range from 2nd to 6th November with transparent red
  geom_rect(aes(xmin = as.Date("2018-11-02"), xmax = as.Date("2018-11-08"), 
                ymin = -Inf, ymax = Inf), 
            fill = "coral", alpha = 0.5, color = NA) +
  geom_line(size = 1.2) +  # Smooth lines
  geom_point(size = 2, alpha = 0.7) +  # Points for daily values
  scale_color_manual(values = c("Delhi" = "red", "Kerala" = "blue")) +  # Custom colors
  labs(
    title = "PM2.5 Levels in November 2018",
    subtitle = "Delhi vs Kerala",
    x = "Date",
    y = "PM2.5 Concentration (µg/m³)",
    color = "State"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "top"
  ) +
  scale_x_date(date_labels = "%d-%b", date_breaks = "1 week") +  # Better date labels
  geom_smooth(method = "loess", se = FALSE, linetype = "dotted", size = 0.8)  # Trend lines

# Print the plot
print(pm25_plot)

