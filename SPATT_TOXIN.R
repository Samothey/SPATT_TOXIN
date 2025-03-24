library(readxl)

# Read Excel file
spatt_data <- read_excel("~/Desktop/SPATT_TOXIN/SPATT_DATA/WY0_SP_2025.xlsx")

names(spatt_data)


# Define the conversion function (just converts one value or vector)
convert_spatt <- function(ng_per_spatt, resin_mass_g = 3) {
  (ng_per_spatt / resin_mass_g) / 1000
}

# Apply to each relevant column and assign to existing or new columns
spatt_data$LF_ug_g <- convert_spatt(spatt_data$LF_ng_SPATT)
spatt_data$NOD_ug_g <- convert_spatt(spatt_data$NOD)
spatt_data$RR_ug_g <- convert_spatt(spatt_data$RR_ng_SPATT)
spatt_data$YR_ug_g <- convert_spatt(spatt_data$YR_ng_SPATT)
spatt_data$WR_ug_g <- convert_spatt(spatt_data$WR_ng_SPATT)
spatt_data$LA_ug_g   <- convert_spatt(spatt_data$LA_ng_SPATT)
spatt_data$LR_ug_g   <- convert_spatt(spatt_data$LR_ng_SPATT)
spatt_data$dmLR_ug_g <- convert_spatt(spatt_data$dmLR_ng_SPATT)
spatt_data$LY_ug_g   <- convert_spatt(spatt_data$LY_ng_SPATT)

ng_per_g <- function(ng_per_spatt, resin_mass_g = 3) {
  ng_per_spatt / resin_mass_g
}

spatt_data$LF_ng_g   <- ng_per_g(spatt_data$LF_ng_SPATT)
spatt_data$NOD_ng_g  <- ng_per_g(spatt_data$NOD)
spatt_data$RR_ng_g   <- ng_per_g(spatt_data$RR_ng_SPATT)
spatt_data$YR_ng_g   <- ng_per_g(spatt_data$YR_ng_SPATT)
spatt_data$WR_ng_g   <- ng_per_g(spatt_data$WR_ng_SPATT)
spatt_data$LA_ng_g   <- ng_per_g(spatt_data$LA_ng_SPATT)
spatt_data$LR_ng_g   <- ng_per_g(spatt_data$LR_ng_SPATT)
spatt_data$dmLR_ng_g <- ng_per_g(spatt_data$dmLR_ng_SPATT)
spatt_data$LY_ng_g   <- ng_per_g(spatt_data$LY_ng_SPATT)



# View the updated data
View(spatt_data)

### Plot 

library(tidyverse)
library(lubridate)
library(stringr)

spatt_data <- spatt_data %>%
  filter(!str_detect(Site_name, "Boysen"))

# Step 1: Format date and extract month
spatt_data <- spatt_data %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = format(Date, "%b"))

# Step 2: Pivot ng/g columns to long format
spatt_long <- spatt_data %>%
  select(Site_name, month, LF_ng_g, NOD_ng_g, RR_ng_g, YR_ng_g, WR_ng_g,
         LA_ng_g, LR_ng_g, dmLR_ng_g, LY_ng_g) %>%
  pivot_longer(cols = ends_with("_ng_g"),
               names_to = "toxin",
               values_to = "load_ng_g")

# Step 3: Order months (optional but helpful)
spatt_long$month <- factor(spatt_long$month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct"))

# Step 4: Plot
ggplot(spatt_long, aes(x = month, y = load_ng_g, fill = toxin)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Site_name) +
  scale_fill_brewer(palette = "Set3") +  
  labs(
    title = "Monthly Relative Toxin Load per Gram of Resin by Site",
    x = "Month",
    y = "Toxin Load (ng/g resin)",
    fill = "Toxin"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



########Create a Presence/Absence Heatmap #######


library(tidyverse)
library(stringr)


spatt_data <- spatt_data %>%
  mutate(
    LA_detected   = ifelse(LA_ng_g > 0, 1, 0),
    LR_detected   = ifelse(LR_ng_g > 0, 1, 0),
    YR_detected   = ifelse(YR_ng_g > 0, 1, 0),
    dmLR_detected = ifelse(dmLR_ng_g > 0, 1, 0),
    LY_detected   = ifelse(LY_ng_g > 0, 1, 0),
    LF_detected   = ifelse(LF_ng_g > 0, 1, 0),
    NOD_detected  = ifelse(NOD_ng_g > 0, 1, 0),
    RR_detected   = ifelse(RR_ng_g > 0, 1, 0),
    WR_detected   = ifelse(WR_ng_g > 0, 1, 0)
  )
presence_long <- spatt_data %>%
  select(Site_name, month, ends_with("_detected")) %>%
  pivot_longer(cols = ends_with("_detected"),
               names_to = "toxin",
               values_to = "presence") %>%
  mutate(toxin = str_replace(toxin, "_detected", ""))  # Optional: clean toxin names
ggplot(presence_long, aes(x = month, y = Site_name, fill = factor(presence))) +
  geom_tile(color = "white", linewidth = 0.5) +
  facet_wrap(~toxin, ncol = 3) +
  scale_fill_manual(values = c("0" = "gray", "1" = "#1f77b4"), labels = c("Not Detected", "Detected")) +
  labs(
    title = "Presence/Absence of Cyanotoxins in SPATT Samples",
    x = "Month",
    y = "Site",
    fill = "Detection"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )



library(tidyverse)
library(stringr)

# Step 1: Create detection columns (1 if toxin > 0, 0 otherwise)
spatt_data <- spatt_data %>%
  mutate(
    LA_detected   = ifelse(LA_ng_g > 0, 1, 0),
    LR_detected   = ifelse(LR_ng_g > 0, 1, 0),
    YR_detected   = ifelse(YR_ng_g > 0, 1, 0),
    dmLR_detected = ifelse(dmLR_ng_g > 0, 1, 0),
    LY_detected   = ifelse(LY_ng_g > 0, 1, 0),
    LF_detected   = ifelse(LF_ng_g > 0, 1, 0),
    NOD_detected  = ifelse(NOD_ng_g > 0, 1, 0),
    RR_detected   = ifelse(RR_ng_g > 0, 1, 0),
    WR_detected   = ifelse(WR_ng_g > 0, 1, 0)
  )

# Step 2: Pivot data to long format and create 'toxin_color' column
presence_long <- spatt_data %>%
  select(Site_name, month, ends_with("_detected")) %>%
  pivot_longer(cols = ends_with("_detected"),
               names_to = "toxin",
               values_to = "presence") %>%
  mutate(
    toxin = str_replace(toxin, "_detected", ""),
    toxin_color = ifelse(presence == 1, toxin, NA)
  )

# Step 3: Define custom colors for each toxin
custom_toxin_colors <- c(
  "LA"   = "#e41a1c",  # red
  "LR"   = "#377eb8",  # blue
  "YR"   = "#4daf4a",  # green
  "dmLR" = "#984ea3",  # purple
  "LY"   = "#ff7f00",  # orange
  "LF"   = "#ffff33",  # yellow
  "NOD"  = "#a65628",  # brown
  "RR"   = "#f781bf",  # pink
  "WR"   = "#999999"   # gray
)

# Step 4: Convert toxin_color to a factor to match color palette
presence_long$toxin_color <- factor(presence_long$toxin_color, levels = names(custom_toxin_colors))

# Step 5: Plot the heatmap
ggplot(presence_long, aes(x = month, y = Site_name, fill = toxin_color)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = custom_toxin_colors,
    na.value = "white",  # tiles stay white if no toxin detected
    name = "Detected Toxin"
  ) +
  labs(
    title = "Toxin-Specific Detection in SPATT Samples",
    x = "Month",
    y = "Site"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )



