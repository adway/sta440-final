library(tidyverse)
library(rstan)
library(sf)
library(terra)
library(tigris)
library(brms)


data <- read_xlsx("data.xlsx")

# Recode variables of interest
data$latitude <- data$`Block Lat`/10^6
data$longitude <- data$`Block Lon`/10^6
data$quality_of_life <- data$`Q3[06]  [6  Overall quality of life in D`
data$quality_of_life <- as.factor(data$quality_of_life)
data$own_rent <- as.factor(data$`Q30  Do you own or rent your current res`)
data$length_cat <- cut(data$`Q27  Approximately how many years have y`, breaks = c(-Inf, 5, 10, 15, 20, 30, Inf),
                   labels = c("0-5", "6-10", "11-15", "16-20", "21-30", "31+"),
                   right = TRUE)
data$length <- data$`Q27  Approximately how many years have y`
data$income <- as.factor(data$`Q33  Would you say your total annual hou`)

data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326) 

durham_blocks <- blocks(state = "NC", county = "Durham", year = 2020, class = "sf")

ggplot() +
  geom_sf(data = durham_blocks, fill = "white", color = "black") + 
  geom_sf(data = data_sf, aes(color = length_cat), size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Latitude", y = "Longitude", color = "Zip Code") + 
  theme_minimal()

ggplot() +
  geom_sf(data = durham_blocks, fill = "white", color = "black") + 
  geom_sf(data = data_sf, aes(color = own_rent), size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Latitude", y = "Longitude", color = "Rent vs. Own") + 
  theme_minimal()

ggplot() +
  geom_sf(data = durham_blocks, fill = "white", color = "black") + 
  geom_sf(data = data_sf, aes(color = income), size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Latitude", y = "Longitude", color = "Income Level") + 
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
data$race <- as.factor(if_else(is.na(data$`Q32[02]  [Black or African American]`), 0, 1))


## Check missing values vs. Zip code
table(data$own_rent, data$zip_recode) # can remove missing values 
table(data$quality_of_life, data$zip_recode) # can remove missing values
data_clean <- data %>%
  filter(quality_of_life != 9 & own_rent != 9 & income != 9)
data_clean$quality_of_life <- as.numeric(data_clean$quality_of_life)

### Bayesian model
fit1 <- brm(quality_of_life ~ 1 + length, 
            family = cumulative(link = "logit", threshold = "flexible"), data = data_clean)

fit2 <- brm(quality_of_life ~ length + (1 | zip_recode), 
            family = cumulative(link = "logit", threshold = "flexible"), data = data_clean)

fit3 <- brm(quality_of_life ~ length + own_rent + length*own_rent + race + income + (1 | zip_recode), 
            family = cumulative(link = "logit", threshold = "flexible"), data = data_clean)

fit4 <- brm(quality_of_life ~ income + race + (1 + length + own_rent + length*own_rent | zip_recode), 
            family = cumulative(link = "logit", threshold = "flexible"), data = data_clean)


exp(-3.64)/(1 + exp(-3.64))
exp(-1.71)/(1 + exp(-1.71))
exp(-0.53)/(1 + exp(-0.53))
exp(1.68)/(1 + exp(1.68))
