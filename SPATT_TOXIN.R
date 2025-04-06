# Load packages
library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)

# === 1. Load SPATT Excel file ===
spatt_data <- read_excel("~/Desktop/spatt_toxin_april/WY0_SP_2025-2.xlsx")


# === 3. Remove Boysen sites ===
spatt_data <- spatt_data %>%
  filter(!str_detect(Site_name, regex("Boysen|BO_|BBR", ignore_case = TRUE)))

# === 4. Calculate toxin loads ===
toxins <- c("LA", "LR", "YR", "dmLR", "LY", "LF", "NOD", "RR", "WR")

for (toxin in toxins) {
  ng_g_col <- paste0(toxin, "_ng_g")
  ug_g_col <- paste0(toxin, "_ug_g")
  
  spatt_data[[ng_g_col]] <- spatt_data[[toxin]] / 3
  spatt_data[[ug_g_col]] <- spatt_data[[ng_g_col]] / 1000
}

# === 5. Pivot longer + clean ===
spatt_long <- spatt_data %>%
  pivot_longer(
    cols = ends_with("_ng_g"),
    names_to = "toxin",
    values_to = "ng_per_g"
  ) %>%
  mutate(
    toxin = str_remove(toxin, "_ng_g"),
    Date = as.Date(Date),
    label_date = format(Date, "%b %d")  # for axis labels (e.g., "Jul 31")
  )

# === 6. Plot with x = Date (for correct order), but readable labels ===
ggplot(spatt_long, aes(x = Date, y = ng_per_g, fill = toxin)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Site_name, scales = "free_y") +
  scale_x_date(date_labels = "%b %d", breaks = unique(spatt_long$Date)) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "SPATT Toxin Load by Site and Date",
    x = "Sampling Date",
    y = "Toxin Load (ng/g)",
    fill = "Toxin"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### heat map####

# Load packages
library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)

# === 1. Load SPATT Excel file ===
spatt_data <- read_excel("~/Desktop/spatt_toxin_april/WY0_SP_2025-2.xlsx")

# === 2. Remove Boysen sites ===
spatt_data <- spatt_data %>%
  filter(!str_detect(Site_name, regex("Boysen|BO_|BBR", ignore_case = TRUE)))

# === 3. Calculate toxin loads ===
toxins <- c("LA", "LR", "YR", "dmLR", "LY", "LF", "NOD", "RR", "WR")

for (toxin in toxins) {
  ng_g_col <- paste0(toxin, "_ng_g")
  ug_g_col <- paste0(toxin, "_ug_g")
  
  spatt_data[[ng_g_col]] <- spatt_data[[toxin]] / 3
  spatt_data[[ug_g_col]] <- spatt_data[[ng_g_col]] / 1000
}

# === 4. Pivot longer ===
spatt_long <- spatt_data %>%
  pivot_longer(
    cols = ends_with("_ng_g"),
    names_to = "toxin",
    values_to = "ng_per_g"
  ) %>%
  mutate(
    toxin = str_remove(toxin, "_ng_g"),
    Date = as.Date(Date),
    label_date = format(Date, "%b %d")
  )

# === 5. Create toxin_color only if ng_per_g > 0 (detected)
spatt_detected <- spatt_long %>%
  mutate(toxin_detected = ifelse(ng_per_g > 0, toxin, NA))

# === 6. Optional: Custom toxin color palette
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

# Ensure toxin levels match the palette order
spatt_detected$toxin_detected <- factor(spatt_detected$toxin_detected, levels = names(custom_toxin_colors))

# === 7. Heatmap Plot ===
ggplot(spatt_detected %>% filter(!is.na(toxin_detected)),
       aes(x = Date, y = Site_name, fill = toxin_detected)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(values = custom_toxin_colors, na.value = "white") +
  scale_x_date(date_labels = "%b %d", breaks = unique(spatt_detected$Date)) +
  labs(
    title = "Toxin Detection by Site and Sampling Date",
    x = "Sampling Date",
    y = "Site",
    fill = "Toxin Detected"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

