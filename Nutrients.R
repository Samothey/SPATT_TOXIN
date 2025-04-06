# Install necessary packages

install.packages("readxl")
install.packages("dplyr")
install.packages("remotes")

##############NUTRIENTS#######################
#### APRIL 6~ CODE TN and AMMONIA CODE######

# Load libraries
library(tidyverse)
library(lubridate)
library(readr)
library(readxl)
# === 1. Load data ===
nutrients <- read_csv("~/Desktop/spatt_toxin_april/LakeData_Brooks2009-2024_Rstudio.csv")

# === 2. Clean and filter ===
nutrients_clean <- nutrients %>%
  mutate(
    date = mdy(date),
    site = tolower(site),
    type = tolower(type)
  ) %>%
  filter(year(date) == 2024,
         str_detect(site, "deepest"),      # Keep deepest site
         type %in% c("bottom", "surface")) # Compare both types

# === 3. Reshape: All values ===
long_all <- nutrients_clean %>%
  select(date, month, waterbody, type, ammonia, TN) %>%
  pivot_longer(cols = c(ammonia, TN), names_to = "Nutrient", values_to = "Value") %>%
  mutate(Detection = "All values")

# === 4. Reshape: Detected only ===
long_detected <- nutrients_clean %>%
  filter(ammonia_cen == FALSE | TN_cen == FALSE) %>%
  mutate(
    ammonia = ifelse(ammonia_cen == FALSE, ammonia, NA),
    TN = ifelse(TN_cen == FALSE, TN, NA)
  ) %>%
  select(date, month, waterbody, type, ammonia, TN) %>%
  pivot_longer(cols = c(ammonia, TN), names_to = "Nutrient", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  mutate(Detection = "Detected only")

# === 5. Combine both ===
combined <- bind_rows(long_all, long_detected)

# Order months for better plotting
combined$month <- factor(combined$month,
                         levels = c("January", "February", "March", "April", "May", "June",
                                    "July", "August", "September", "October", "November", "December"),
                         ordered = TRUE)

##plot 1 TN and ammonia - all vaules- (bottom vs. surface) #######
ggplot(combined %>% filter(Detection == "All values"),
       aes(x = month, y = Value, color = type, group = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_grid(Nutrient ~ waterbody, scales = "free_y") +  # 👈 Add this!
  scale_color_manual(values = c("bottom" = "steelblue", "surface" = "darkorange"),
                     labels = c("Bottom", "Surface")) +
  labs(title = "Ammonia and TN by Month and Depth (All Values)",
       x = "Month", y = "Concentration (mg/L)", color = "Sample Type") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Plot 2: TN and ammomnia -detect only (surafc vs. bottom) #########
ggplot(combined %>% filter(Detection == "Detected only"),
       aes(x = month, y = Value, color = type, group = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_grid(Nutrient ~ waterbody, scales = "free_y") +
  scale_color_manual(values = c("bottom" = "steelblue", "surface" = "darkorange"),
                     labels = c("Bottom", "Surface")) +
  labs(title = "Ammonia and TN by Month and Depth (Detected Only)",
       x = "Month", y = "Concentration (mg/L)", color = "Sample Type") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## New code-- Only Bottom samples of TN and Ammonia

# Load required packages
library(tidyverse)
library(lubridate)

# === 1. Load the data ===
nutrients <- read_csv("~/Desktop/spatt_toxin_april/LakeData_Brooks2009-2024_Rstudio.csv")

# === 2. Clean and filter: 2024, deepest, bottom only ===
nutrients_bottom <- nutrients %>%
  mutate(
    date = mdy(date),
    site = tolower(site),
    type = tolower(type)
  ) %>%
  filter(
    year(date) == 2024,
    str_detect(site, "deepest"),
    type == "bottom"
  )

# === 3. All values (bottom only) ===
long_all <- nutrients_bottom %>%
  select(date, month, waterbody, ammonia, TN) %>%
  pivot_longer(cols = c(ammonia, TN),
               names_to = "Nutrient",
               values_to = "Value") %>%
  mutate(Detection = "All values")

# === 4. Detected only (ammonia_cen or TN_cen is FALSE) ===
long_detected <- nutrients_bottom %>%
  filter(ammonia_cen == FALSE | TN_cen == FALSE) %>%
  mutate(
    ammonia = ifelse(ammonia_cen == FALSE, ammonia, NA),
    TN = ifelse(TN_cen == FALSE, TN, NA)
  ) %>%
  select(date, month, waterbody, ammonia, TN) %>%
  pivot_longer(cols = c(ammonia, TN),
               names_to = "Nutrient",
               values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  mutate(Detection = "Detected only")

# === 5. Combine both datasets ===
combined <- bind_rows(long_all, long_detected)

# Optional: format month as ordered factor
combined$month <- factor(combined$month,
                         levels = c("January", "February", "March", "April", "May", "June",
                                    "July", "August", "September", "October", "November", "December"),
                         ordered = TRUE)
 ## Plot 4: all samples 
ggplot(combined %>% filter(Detection == "All values"),
       aes(x = month, y = Value, color = Nutrient, group = Nutrient)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ waterbody, scales = "free_y") +
  scale_color_manual(values = c("ammonia" = "steelblue", "TN" = "darkorange")) +
  labs(title = "Bottom Ammonia and TN by Month (All Values)",
       x = "Month", y = "Concentration (mg/L)", color = "Nutrient") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Detect only (bottom)
ggplot(combined %>% filter(Detection == "Detected only"),
       aes(x = month, y = Value, color = Nutrient, group = Nutrient)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ waterbody, scales = "free_y") +
  scale_color_manual(values = c("ammonia" = "steelblue", "TN" = "darkorange")) +
  labs(title = "Bottom Ammonia and TN by Month (Detected Only)",
       x = "Month", y = "Concentration (mg/L)", color = "Nutrient") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Chlorophyll-a vs. Ammonia vs. TN######

# Load libraries
library(tidyverse)
library(lubridate)

# === 1. Load and clean data ===
nutrients <- read_csv("~/Desktop/spatt_toxin_april/LakeData_Brooks2009-2024_Rstudio.csv")

nutrients_clean <- nutrients %>%
  mutate(
    date = mdy(date),
    site = tolower(site),
    type = tolower(type)
  ) %>%
  filter(year(date) == 2024, str_detect(site, "deepest"))

# === 2. Bottom ammonia + TN ===
bottom_nutrients <- nutrients_clean %>%
  filter(type == "bottom") %>%
  select(date, month, waterbody, ammonia, TN) %>%
  pivot_longer(cols = c(ammonia, TN), names_to = "Variable", values_to = "Value") %>%
  mutate(Type = "Bottom")

# === 3. Surface Chla ===
surface_chla <- nutrients_clean %>%
  filter(type == "surface") %>%
  select(date, month, waterbody, chla) %>%
  mutate(Type = "Surface", Variable = "chla", Value = chla) %>%
  select(-chla)

# === 4. Combine and clean ===
combined_corrected <- bind_rows(bottom_nutrients, surface_chla) %>%
  mutate(
    Variable = tolower(Variable),
    Category = case_when(
      Variable %in% c("ammonia", "tn") ~ "Nutrients",
      Variable == "chla" ~ "Chlorophyll-a"
    )
  )

# === 5. Order months ===
combined_corrected$month <- factor(combined_corrected$month,
                                   levels = c("January", "February", "March", "April", "May", "June",
                                              "July", "August", "September", "October", "November", "December"),
                                   ordered = TRUE)

# === 6. Plot: Ammonia + TN together, Chla separate ===
ggplot(combined_corrected, aes(x = month, y = Value, color = Variable, group = Variable)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_grid(Category ~ waterbody, scales = "free_y") +
  scale_color_manual(
    values = c("ammonia" = "#1f78b4", "tn" = "#e31a1c", "chla" = "#33a02c"),
    labels = c("ammonia" = "Ammonia", "tn" = "Total Nitrogen", "chla" = "Chlorophyll-a")
  ) +
  labs(
    title = "Nutrient and Chlorophyll-a Trends by Month",
    x = "Month", y = "Concentration", color = "Variable"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



### ortho p and TP###

library(tidyverse)
library(lubridate)

# === 1. Load data ===
nutrients <- read_csv("~/Desktop/spatt_toxin_april/LakeData_Brooks2009-2024_Rstudio.csv")

# === 2. Filter for 2024, deepest site only ===
nutrients_clean <- nutrients %>%
  mutate(
    date = mdy(date),
    site = tolower(site),
    type = tolower(type)
  ) %>%
  filter(
    year(date) == 2024,
    str_detect(site, "deepest"),
    type %in% c("bottom", "surface")
  )

# === 3. Reshape to long format: OrthoP + TP ===
phosphorus_long <- nutrients_clean %>%
  select(date, month, waterbody, type, orthoP, TP) %>%
  pivot_longer(cols = c(orthoP, TP),
               names_to = "Variable", values_to = "Value")

# === 4. Order months ===
phosphorus_long$month <- factor(phosphorus_long$month,
                                levels = c("January", "February", "March", "April", "May", "June",
                                           "July", "August", "September", "October", "November", "December"),
                                ordered = TRUE)
## ortho P ##
ggplot(phosphorus_long %>% filter(Variable == "orthoP"),
       aes(x = month, y = Value, color = type, group = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ waterbody, scales = "free_y") +
  scale_color_manual(values = c("bottom" = "#1f78b4", "surface" = "#33a02c"),
                     labels = c("bottom" = "Bottom", "surface" = "Surface")) +
  labs(
    title = "Orthophosphate Concentrations by Depth (2024, Deepest Sites)",
    x = "Month", y = "Concentration (mg/L)", color = "Sample Type"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#####TP####
ggplot(phosphorus_long %>% filter(Variable == "TP"),
       aes(x = month, y = Value, color = type, group = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ waterbody, scales = "free_y") +
  scale_color_manual(values = c("bottom" = "#e31a1c", "surface" = "#33a02c"),
                     labels = c("bottom" = "Bottom", "surface" = "Surface")) +
  labs(
    title = "Total Phosphorus Concentrations by Depth (2024, Deepest Sites)",
    x = "Month", y = "Concentration (mg/L)", color = "Sample Type"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
