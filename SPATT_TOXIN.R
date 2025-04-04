library(readxl)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)

##First Round of SPATT DATA BROOKS 2024####
########Reading in data and graphs########

# Read Excel file
spatt_data <- read_excel("~/Desktop/spatt_toxin_april/WY0_SP_2025-2.xlsx")

# Create ng/g and µg/g columns
toxins <- c("LA", "LR", "YR", "dmLR", "LY", "LF", "NOD", "RR", "WR")

for (toxin in toxins) {
  ng_g_col <- paste0(toxin, "_ng_g")
  ug_g_col <- paste0(toxin, "_ug_g")
  
  spatt_data[[ng_g_col]] <- spatt_data[[toxin]] / 3
  spatt_data[[ug_g_col]] <- spatt_data[[ng_g_col]] / 1000
}

# Filter out BO_BBR sites
spatt_data <- spatt_data %>%
  filter(!str_detect(Site_name, "Boysen"))

# Pivot to long format and clean up toxin names
spatt_long <- spatt_data %>%
  pivot_longer(
    cols = ends_with("_ng_g"),
    names_to = "toxin",
    values_to = "ng_per_g"
  ) %>%
  mutate(
    toxin = str_replace(toxin, "_ng_g", ""),
    Date = as.Date(Date),
    month = format(Date, "%b"),
    month = factor(month, levels = c("Jul", "Aug", "Sep", "Oct"))
  )

# Plot: stacked bar of total toxin ng/g per site per month W/ Free-y
ggplot(spatt_long, aes(x = month, y = ng_per_g, fill = toxin)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Site_name, scales = "free_y") +
  labs(
    title = "Monthly Toxin Load per Gram of Resin by Site",
    x = "Month",
    y = "Toxin Load (ng/g)",
    fill = "Toxin"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot: stacked bar of total toxin ng/g per site per month W/oFree-y
ggplot(spatt_long, aes(x = month, y = ng_per_g, fill = toxin)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Site_name) +
  labs(
    title = "Monthly Toxin Load per Gram of Resin by Site",
    x = "Month",
    y = "Toxin Load (ng/g)",
    fill = "Toxin"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##heatmap

# Step 1: Create toxin_color column (if not already present)
spatt_long <- spatt_long %>%
  mutate(toxin_color = ifelse(ng_per_g > 0, toxin, NA))

# Step 2: Filter for detected toxins only
detected_only <- spatt_long %>%
  filter(!is.na(toxin_color))

# Step 3: Define custom colors (run this before plotting!)
custom_toxin_colors <- c(
  "LA"   = "#1f77b4",
  "LR"   = "#ff7f0e",
  "YR"   = "#2ca02c",
  "dmLR" = "#d62728",
  "LY"   = "#9467bd",
  "LF"   = "#8c564b",
  "NOD"  = "#e377c2",
  "RR"   = "#7f7f7f",
  "WR"   = "#17becf"
)

# Step 4: Convert to factor for color mapping
detected_only$toxin_color <- factor(detected_only$toxin_color, levels = names(custom_toxin_colors))

# Step 5: Plot!
ggplot(detected_only, aes(x = month, y = Site_name, fill = toxin_color)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(values = custom_toxin_colors, na.value = "white") +
  labs(
    title = "Toxin Detection by Site and Month",
    x = "Month",
    y = "Site",
    fill = "Toxin"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot: stacked bar of total toxin ng/g per site per month W/oFree-y
ggplot(spatt_long, aes(x = month, y = ng_per_g, fill = toxin)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Site_name) +
  labs(
    title = "Monthly Toxin Load per Gram of Resin by Site",
    x = "Month",
    y = "Toxin Load (ng/g)",
    fill = "Toxin"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))