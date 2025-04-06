# Load necessary packages
library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)


# === 1. Load and clean data ===
strat <- read_excel("~/Desktop/spatt_toxin_april/lakestratification.xlsx")

strat_clean <- strat %>%
  mutate(
    Date = as.Date(Date),
    DO = as.numeric(str_remove(as.character(DO), "\\*"))  # remove asterisks
  )

# === 2. Define plot function for one lake ===
plot_profiles_faceted <- function(data, lake_name) {
  lake_data <- data %>% filter(Waterbody == lake_name)
  
  lake_long <- lake_data %>%
    pivot_longer(cols = c(Temp, DO), names_to = "Variable", values_to = "Value")
  
  ggplot(lake_long, aes(x = Value, y = Depth, color = as.factor(Date))) +
    geom_line(size = 1.2, na.rm = TRUE) +
    geom_point(size = 2, na.rm = TRUE) +
    scale_y_reverse() +
    facet_wrap(~ Variable, scales = "free_x") +
    labs(
      title = paste("Temperature & DO Stratification -", lake_name),
      x = "Temperature (°C) or DO (mg/L)",
      y = "Depth (m)",
      color = "Date"
    ) +
    theme_minimal() +
    theme(strip.text = element_text(size = 12, face = "bold"))
}

# === 3. Loop through and display plots ===
unique_lakes <- unique(strat_clean$Waterbody)

walk(unique_lakes, function(lake) {
  message("Displaying: ", lake)
  print(plot_profiles_faceted(strat_clean, lake))
})
