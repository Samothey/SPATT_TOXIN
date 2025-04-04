# Install necessary packages

install.packages("readxl")
install.packages("dplyr")
install.packages("remotes")

# Load libraries
library(readxl)
library(dplyr)
library(rLakeAnalyzer)

# Load your Excel data
upperbrooks <- read_excel("lakestratification.xlsx", sheet = "upperbrooks") %>%
  select(Date, Depth, Temp)

rainbow <- read_excel("lakestratification.xlsx", sheet = "rainbow") %>%
  select(Date, Depth, Temp)

# Function to calculate thermocline depth by date
get_thermocline_depth <- function(df) {
  df %>%
    group_by(Date) %>%
    group_modify(~ {
      depths <- .x$Depth
      temps <- .x$Temp
      tryCatch({
        td <- thermo.depth(wtr = temps, depths = depths)
        tibble(ThermoclineDepth = td)
      }, error = function(e) {
        tibble(ThermoclineDepth = NA)
      })
    }) %>%
    ungroup()
}
library(ggplot2)
library(viridis)
library(dplyr)
library(readxl)


# Load the data
upperbrooks <- read_excel("lakestratification.xlsx", sheet = "upperbrooks") %>%
  select(Date, Depth, Temp)

# Filter for only depths 0 and 4.5
temps_filtered <- upperbrooks %>%
  filter(Depth %in% c(0, 4.5))

# Plot
ggplot(temps_filtered, aes(x = as.Date(Date), y = Temp, color = as.factor(Depth))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("0" = "steelblue", "4.5" = "firebrick"),
                     name = "Depth (m)",
                     labels = c("Surface (0 m)", "Deep (4.5 m)")) +
  labs(title = "Surface vs Deep Water Temperature at Upper Brooks Lake",
       x = "Date", y = "Temperature (°C)") +
  theme_minimal()

library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)

# Load and clean data
upperbrooks <- read_excel("lakestratification.xlsx", sheet = "upperbrooks") %>%
  select(Date, Depth, do)

# Filter for depths 0 and 4.5
do_depths <- upperbrooks %>%
  filter(Depth %in% c(0, 4.5)) %>%
  mutate(Depth = as.factor(Depth))  # convert depth to factor for plotting

# Plot
ggplot(do_depths, aes(x = as.Date(Date), y = do, color = Depth)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("0" = "darkblue", "4.5" = "red"),
                     labels = c("Surface (0 m)", "Bottom (4.5 m)"),
                     name = "Depth") +
  labs(title = "Dissolved Oxygen at Surface and Bottom (Upper Brooks Lake)",
       x = "Date", y = "Dissolved Oxygen (mg/L)") +
  theme_minimal()

#####################################

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# Load data
data <- read_csv("~/Desktop/spatt_toxin_april/LakeData_Brooks2009-2024_Rstudio.csv")
# Convert date column
data$date <- mdy(data$date)

# Filter for bottom samples from 2024 and values ABOVE reporting limit
detected_values <- data %>%
  filter(year(date) == 2024,
         tolower(type) == "bottom") %>%
  filter(ammonia_cen == FALSE | TN_cen == FALSE) %>%
  select(waterbody, date, ammonia, ammonia_cen, TN, TN_cen)

# Convert to long format
long_detected <- detected_values %>%
  mutate(ammonia = ifelse(ammonia_cen == FALSE, ammonia, NA),
         TN = ifelse(TN_cen == FALSE, TN, NA)) %>%
  select(waterbody, date, ammonia, TN) %>%
  pivot_longer(cols = c(ammonia, TN), names_to = "Nutrient", values_to = "Value") %>%
  filter(!is.na(Value))

# Plot
ggplot(long_detected, aes(x = as.Date(date), y = Value, color = Nutrient)) +
  geom_line(aes(group = Nutrient), linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~ waterbody, scales = "free_y") +
  scale_color_manual(values = c("ammonia" = "steelblue", "TN" = "darkorange"),
                     labels = c("Ammonia", "Total Nitrogen")) +
  labs(title = "Detected Ammonia and TN Concentrations (Bottom Samples, 2024)",
       x = "Date", y = "Concentration (mg/L)", color = "Nutrient") +
  theme_bw()

