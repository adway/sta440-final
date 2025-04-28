library(tidyverse)
library(rstan)
library(sf)
library(terra)
library(tigris)


data <- read_xlsx("data.xlsx")

# Recode variables of interest
data$latitude <- data$`Block Lat`/10^6
data$longitude <- data$`Block Lon`/10^6
data$quality_of_life <- data$`Q3[06]  [6  Overall quality of life in D`
data$quality_of_life <- as.factor(data$quality_of_life)
data$own_rent <- as.factor(data$`Q30  Do you own or rent your current res`)
data$length <- cut(data$`Q27  Approximately how many years have y`, breaks = c(-Inf, 5, 10, 15, 20, 30, Inf),
                   labels = c("0-5", "6-10", "11-15", "16-20", "21-30", "31+"),
                   right = TRUE)

data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326) 

durham_blocks <- blocks(state = "NC", county = "Durham", year = 2020, class = "sf")

ggplot() +
  geom_sf(data = durham_blocks, fill = "white", color = "black") + 
  geom_sf(data = data_sf, aes(color = length), size = 2) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()


### Produce supplementary figure 1 -- Zip recode

data$Zip <- as.character(data$Zip)
u5 <- names(which(table(data$Zip) < 15))

data <- data %>%
  mutate(zip_recode = if_else(Zip %in% u5, "other", Zip))

data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326) 

ggplot() +
  geom_sf(data = durham_blocks, fill = "white", color = "black") + 
  geom_sf(data = data_sf, aes(color = zip_recode), size = 2) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

data$`Q32[02]  [Black or African American]`
