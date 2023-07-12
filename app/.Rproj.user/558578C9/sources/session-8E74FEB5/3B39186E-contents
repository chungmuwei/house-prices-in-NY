library(tidyverse)

# Read house data
house <- readr::read_tsv("data/housing-prices-ge19.txt")

# convert data types and remove Test column
house = house %>% mutate (
  Waterfront = as.factor(Waterfront),
  New.Construct = as.factor(New.Construct),
  Central.Air = as.factor(Central.Air),
  Fuel.Type = as.factor(Fuel.Type),
  Heat.Type = as.factor(Heat.Type),
  Sewer.Type = as.factor(Sewer.Type),
) %>% select(-Test)

# all house variables to fit model
full_vars = colnames(house)[2:16] # exclude dependent variable: Price

# the variable our model used 
default_vars = c("Waterfront", "Land.Value", "Living.Area", "Bathrooms")

# choice
fuel_type = c("Electric", "Gas", "Oil", "None")
heat_type = c("Electric", "Hot Air", "Hot Water", "None")
sewer_type = c("Public", "Private", "None/Unknown")

# fit linear regression model
#house_fit = lm(Price ~ Waterfront + Land.Value + Living.Area + Bathrooms, data = house)



