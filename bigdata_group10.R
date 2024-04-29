#### Total used time: 5 min 40 sec

#### Load Packages
library(dplyr)
library(tidyr)
library(DescTools)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(randomForest)

#### Historical_data
### 1 Data Exploration
# load the data as dataframe
data <- read.csv("historic_property_data.csv")

### 2 Data Prepocessing
## 2.1 Variables Selecting
# keep the predictors and outcome variables in data frame
# By codebook: only select the ones which "var_is_predictor" is TRUE
# By observation: meta_town_code and meta_nbhd are hierarchical variables, meta_nbhd is sufficient since the first 2 digits are township code, and last 3 digits are neighborhood code - remove meta_town_code.
# neighborhood characteristics and school district boundaries correlate with each other in real estate assessment as these are related to particular neighborhood. geo_school_elem_district, geo_school_hs_district are removed.
data <- data[, c('sale_price', 'meta_nbhd', 'char_hd_sf', 'char_age', 'char_apts', 'char_ext_wall', 'char_roof_cnst', 'char_rooms', 'char_beds', 'char_bsmt', 'char_bsmt_fin', 'char_heat', 'char_oheat', 'char_air', 'char_frpl', 'char_attic_type', 'char_fbath', 'char_hbath', 'char_tp_plan', 'char_tp_dsgn', 'char_gar1_size', 'char_gar1_cnst', 'char_gar1_att', 'char_gar1_area', 'char_bldg_sf', 'char_use', 'char_type_resd', 'char_attic_fnsh', 'char_porch', 'geo_ohare_noise', 'geo_floodplain', 'geo_fs_flood_factor', 'geo_fs_flood_risk_direction', 'geo_withinmr100', 'geo_withinmr101300', 'econ_tax_rate', 'econ_midincome')]

## 2.2 Check the variables and make sure their type
# categorical variables
data$char_air <- factor(data$char_air, levels = c(1, 2), labels = c("Central A/C", "No Central A/C"))
data$char_apts <- factor(data$char_apts, levels = c(1, 2, 3, 4, 5), labels = c("Two", "Three", "Four", "Five", "Six"))
data$char_attic_fnsh <- factor(data$char_attic_fnsh, levels = c(1, 2, 3), labels = c("Living Area", "Partial", "None"))
data$char_attic_type <- factor(data$char_attic_type, levels = c(1, 2, 3), labels = c("Full", "Partial", "None"))
data$char_bsmt <- factor(data$char_bsmt, levels = c(1, 2, 3, 4), labels = c("Full", "Slab", "Partial", "None"))
data$char_bsmt_fin <- factor(data$char_bsmt_fin, levels = c(1, 2, 3), labels = c("Formal Rec Room", "Apartment", "None"))
data$char_ext_wall <- factor(data$char_ext_wall, levels = c(1, 2, 3), labels = c("Frame", "Masonry", "Frame + Masonry"))
data$char_gar1_area <- factor(data$char_gar1_area, levels = c(1, 2), labels = c("Yes", "No"))
data$char_gar1_att <- factor(data$char_gar1_att, levels = c(1, 2), labels = c("Yes", "No"))
data$char_gar1_cnst <- factor(data$char_gar1_cnst, levels = c(1, 2, 3), labels = c("Frame", "Masonry", "Frame + Masonry"))
data$char_gar1_size <- factor(data$char_gar1_size, levels = c(1, 2, 3, 4), labels = c("1 cars", "1.5 cars", "2 cars", "3 cars"))
data$char_heat <- factor(data$char_heat, levels = c(1, 2, 3), labels = c("Warm Air Furnace", "Hot Water Steam", "Hot Water"))
data$char_oheat <- factor(data$char_oheat, levels = c(1, 2, 3), labels = c("Floor Furnace", "Unit Heater", "No Heat"))
data$char_porch <- factor(data$char_porch, levels = c(1, 2), labels = c("Frame Enclosed", "Masonry Enclosed"))
data$char_roof_cnst <- factor(data$char_roof_cnst, levels = c(1, 2), labels = c("Shingle + Asphalt", "Tar + Gravel"))
data$char_tp_dsgn <- factor(data$char_tp_dsgn, levels = c(1, 2), labels = c("Yes", "No"))
data$char_tp_plan <- factor(data$char_tp_plan, levels = c(1, 2), labels = c("Architect", "Stock Plan"))
data$char_type_resd <- factor(data$char_type_resd, levels = c(1, 2, 3), labels = c("1 Story", "2 Story", "3 Story"))
data$char_use <- factor(data$char_use, levels = c(1, 2), labels = c("Single-Family", "Multi-Family"))

# numeric variables
data$char_age <- as.numeric(data$char_age)
data$char_beds <- as.numeric(data$char_beds)
data$char_bldg_sf <- as.numeric(data$char_bldg_sf)
data$char_fbath <- as.numeric(data$char_fbath)
data$char_frpl <- as.numeric(data$char_frpl)
data$char_hbath <- as.numeric(data$char_hbath)
data$char_hd_sf <- as.numeric(data$char_hd_sf)
data$char_rooms <- as.numeric(data$char_rooms)
data$sale_price <- as.numeric(data$sale_price)
data$econ_tax_rate <- as.numeric(data$econ_tax_rate)
data$econ_midincome <- as.numeric(data$econ_midincome)

## 2.3 Columns Renamed

# rename the columns
colnames(data) <- c('sale_price', 'nbhd_code', 'land_area_sqft', 'property_age', 'number_apts', 'ext_wall_material', 'roof_material', 'number_rooms', 'number_beds', 'basement_type', 'basement_finish', 'central_heating', 'other_heating', 'central_air_conditioning', 'number_fireplaces', 'attic_type', 'number_full_bath', 'number_half_bath', 'design_plan', 'cathedral_ceiling', 'garage_size', 'garage_material', 'garage_attached', 'garage_area', 'buidling_area_sqft', 'usage_type', 'residence_type', 'attic_finish', 'porch_type', 'noise_indicator', 'fema_floodplain', 'flood_risk_factor', 'food_risk_direction', 'road_prox_within_100', 'road_prox_within_101_to_300', 'tax_rate', 'midincome')

## 2.4 Drop variables that have too many missing values, too few unique values, or too many unique values
missing_value_threshold <- 0.1  # Drop columns where more than 10% of the values are NA

# Function to determine columns to keep
columns_to_keep <- function(data) {
  # Calculate the percentage of missing values for each column
  missing_percentage <- sapply(data, function(x) sum(is.na(x)) / length(x))
  
  # Initialize a logical vector to keep track of which columns to keep
  keep <- missing_percentage <= missing_value_threshold
  
  # Loop through each column to apply the unique values check only to numeric columns
  for (col_name in names(data)) {
    if (is.numeric(data[[col_name]])) {
      # Calculate the number of unique non-NA values for this numeric column
      num_unique_values <- length(unique(na.omit(data[[col_name]])))
      
      # Minimum number of unique values (to avoid columns with no variation)
      min_unique_values_threshold <- 5
      
      # Maximum unique values threshold (to avoid columns with too high cardinality)
      max_unique_values_threshold <- nrow(data) * 0.95
      
      # Determine whether to keep this column based on unique values criteria
      keep[col_name] <- keep[col_name] && (num_unique_values >= min_unique_values_threshold) &&
        (num_unique_values <= max_unique_values_threshold)
    }
  }
  
  return(names(data)[keep])
}

# Apply the function to filter the data
filtered_columns <- columns_to_keep(data)
filtered_data <- data[, filtered_columns]

## 2.5 Replace missing values with non-missing values in the same location group
# Function to calculate mode
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  if (length(ux) == 0) return(NA)  # return NA if there are no non-NA values
  ux[which.max(tabulate(match(x, ux)))]
}

# impute missing values based on the median or mode of 'nbhd_code'(neighborhood). Because we assume that the following characteristics can be quite consistent within the same neighborhood.
filtered_data <- filtered_data %>%
  group_by(nbhd_code) %>%
  mutate(
    ext_wall_material = ifelse(is.na(ext_wall_material), get_mode(ext_wall_material), ext_wall_material),
    roof_material = ifelse(is.na(roof_material), get_mode(roof_material), roof_material),
    basement_type = ifelse(is.na(basement_type), get_mode(basement_type), basement_type),
    basement_finish = ifelse(is.na(basement_finish), get_mode(basement_finish), basement_finish),
    central_heating = ifelse(is.na(central_heating), get_mode(central_heating), central_heating),
    central_air_conditioning = ifelse(is.na(central_air_conditioning), get_mode(central_air_conditioning), central_air_conditioning),
    number_fireplaces = ifelse(is.na(number_fireplaces), median(number_fireplaces, na.rm = TRUE), number_fireplaces),
    attic_type = ifelse(is.na(attic_type), get_mode(attic_type), attic_type),
    usage_type = ifelse(is.na(usage_type), get_mode(usage_type), usage_type),
    flood_risk_factor = ifelse(is.na(flood_risk_factor), get_mode(flood_risk_factor), flood_risk_factor),
    midincome = ifelse(is.na(midincome), median(midincome, na.rm = TRUE), midincome)
  )

if (any(is.na(filtered_data$roof_material))) {
  # Calculate the mode of roof_material from non-NA values of the entire dataset
  overall_mode_roof_material <- get_mode(filtered_data$roof_material)
  # Apply this mode to fill remaining NAs
  filtered_data$roof_material[is.na(filtered_data$roof_material)] <- overall_mode_roof_material
}

## 2.6 Winsorize numeric variables
numeric_vars <- c("land_area_sqft", "property_age", "number_rooms", "number_beds", "number_fireplaces", "number_full_bath", "number_half_bath", "buidling_area_sqft")

# Apply Winsorizing to each numeric variable
filtered_data[numeric_vars] <- lapply(filtered_data[numeric_vars], function(x) Winsorize(x, probs = c(0.05, 0.95)))

## 2.7 combine categories to reduce the number of unique groups -- almost all the categories has similar in each group, no need to combine. nbhd_code is at a granular level, so no need to combine

## 2.8 Remove outliers
data <- filtered_data

outliers_sale_price <- abs(scale(data$sale_price)) > 3
data <- data[!outliers_sale_price, ]

# Scale the predictor variables
scaled_df <- data %>%
  select(-sale_price) %>%
  scale()

#Identify and remove outliers in the predictor variables (remove observations where any predictor variable deviates more than 3 standard deviations from the mean)
outliers_predictors <- apply(scaled_df, 1, function(x) any(abs(x) > 3))
data <- data[!outliers_predictors, ]

# ------------------------------------------------------------------------------------------------------------

### Predict_data and Predcit
# 1 Load the data as dataframe
predict_data <- read.csv("predict_property_data.csv")

### 2 Data Prepocessing -- follow the way we dealed with the history data
## 2.1 Variables Selecting
predict_data <- predict_data[, c('pid', 'meta_nbhd', 'char_hd_sf', 'char_age', 'char_apts', 'char_ext_wall', 'char_roof_cnst', 'char_rooms', 'char_beds', 'char_bsmt', 'char_bsmt_fin', 'char_heat', 'char_oheat', 'char_air', 'char_frpl', 'char_attic_type', 'char_fbath', 'char_hbath', 'char_tp_plan', 'char_tp_dsgn', 'char_gar1_size', 'char_gar1_cnst', 'char_gar1_att', 'char_gar1_area', 'char_bldg_sf', 'char_use', 'char_type_resd', 'char_attic_fnsh', 'char_porch', 'geo_ohare_noise', 'geo_floodplain', 'geo_fs_flood_factor', 'geo_fs_flood_risk_direction', 'geo_withinmr100', 'geo_withinmr101300', 'econ_tax_rate', 'econ_midincome')]

# Check the variables and make sure their type
# categorical variables
predict_data$char_air <- factor(predict_data$char_air, levels = c(1, 2), labels = c("Central A/C", "No Central A/C"))
predict_data$char_apts <- factor(predict_data$char_apts, levels = c(1, 2, 3, 4, 5), labels = c("Two", "Three", "Four", "Five", "Six"))
predict_data$char_attic_fnsh <- factor(predict_data$char_attic_fnsh, levels = c(1, 2, 3), labels = c("Living Area", "Partial", "None"))
predict_data$char_attic_type <- factor(predict_data$char_attic_type, levels = c(1, 2, 3), labels = c("Full", "Partial", "None"))
predict_data$char_bsmt <- factor(predict_data$char_bsmt, levels = c(1, 2, 3, 4), labels = c("Full", "Slab", "Partial", "None"))
predict_data$char_bsmt_fin <- factor(predict_data$char_bsmt_fin, levels = c(1, 2, 3), labels = c("Formal Rec Room", "Apartment", "None"))
predict_data$char_ext_wall <- factor(predict_data$char_ext_wall, levels = c(1, 2, 3), labels = c("Frame", "Masonry", "Frame + Masonry"))
predict_data$char_gar1_area <- factor(predict_data$char_gar1_area, levels = c(1, 2), labels = c("Yes", "No"))
predict_data$char_gar1_att <- factor(predict_data$char_gar1_att, levels = c(1, 2), labels = c("Yes", "No"))
predict_data$char_gar1_cnst <- factor(predict_data$char_gar1_cnst, levels = c(1, 2, 3), labels = c("Frame", "Masonry", "Frame + Masonry"))
predict_data$char_gar1_size <- factor(predict_data$char_gar1_size, levels = c(1, 2, 3, 4), labels = c("1 cars", "1.5 cars", "2 cars", "3 cars"))
predict_data$char_heat <- factor(predict_data$char_heat, levels = c(1, 2, 3), labels = c("Warm Air Furnace", "Hot Water Steam", "Hot Water"))
predict_data$char_oheat <- factor(predict_data$char_oheat, levels = c(1, 2, 3), labels = c("Floor Furnace", "Unit Heater", "No Heat"))
predict_data$char_porch <- factor(predict_data$char_porch, levels = c(1, 2), labels = c("Frame Enclosed", "Masonry Enclosed"))
predict_data$char_roof_cnst <- factor(predict_data$char_roof_cnst, levels = c(1, 2), labels = c("Shingle + Asphalt", "Tar + Gravel"))
predict_data$char_tp_dsgn <- factor(predict_data$char_tp_dsgn, levels = c(1, 2), labels = c("Yes", "No"))
predict_data$char_tp_plan <- factor(predict_data$char_tp_plan, levels = c(1, 2), labels = c("Architect", "Stock Plan"))
predict_data$char_type_resd <- factor(predict_data$char_type_resd, levels = c(1, 2, 3), labels = c("1 Story", "2 Story", "3 Story"))
predict_data$char_use <- factor(predict_data$char_use, levels = c(1, 2), labels = c("Single-Family", "Multi-Family"))

# numeric variables
predict_data$char_age <- as.numeric(predict_data$char_age)
predict_data$char_beds <- as.numeric(predict_data$char_beds)
predict_data$char_bldg_sf <- as.numeric(predict_data$char_bldg_sf)
predict_data$char_fbath <- as.numeric(predict_data$char_fbath)
predict_data$char_frpl <- as.numeric(predict_data$char_frpl)
predict_data$char_hbath <- as.numeric(predict_data$char_hbath)
predict_data$char_hd_sf <- as.numeric(predict_data$char_hd_sf)
predict_data$char_rooms <- as.numeric(predict_data$char_rooms)
predict_data$pid <- as.numeric(predict_data$pid)
predict_data$econ_tax_rate <- as.numeric(predict_data$econ_tax_rate)
predict_data$econ_midincome <- as.numeric(predict_data$econ_midincome)

# rename the columns
colnames(predict_data) <- c('pid', 'nbhd_code', 'land_area_sqft', 'property_age', 'number_apts', 'ext_wall_material', 'roof_material', 'number_rooms', 'number_beds', 'basement_type', 'basement_finish', 'central_heating', 'other_heating', 'central_air_conditioning', 'number_fireplaces', 'attic_type', 'number_full_bath', 'number_half_bath', 'design_plan', 'cathedral_ceiling', 'garage_size', 'garage_material', 'garage_attached', 'garage_area', 'buidling_area_sqft', 'usage_type', 'residence_type', 'attic_finish', 'porch_type', 'noise_indicator', 'fema_floodplain', 'flood_risk_factor', 'food_risk_direction', 'road_prox_within_100', 'road_prox_within_101_to_300', 'tax_rate', 'midincome')
str(predict_data)

# Find common columns between test.df and predict_data
common_cols <- intersect(names(data), names(predict_data))

# Include "pid"
common_cols <- c("pid", setdiff(common_cols, "pid"))

# Select columns in predict_data that are also in training
predict_data <- predict_data[, common_cols]

## 2.2 Replace missing values with non-missing values in the same location group
# Function to calculate mode
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  if (length(ux) == 0) return(NA)  # return NA if there are no non-NA values
  ux[which.max(tabulate(match(x, ux)))]
}

# impute missing values based on the median or mode of 'nbhd_code'(neighborhood). Because we assume that the following characteristics can be quite consistent within the same neighborhood.
predict_data <- predict_data %>%
  group_by(nbhd_code) %>%
  mutate(
    ext_wall_material = ifelse(is.na(ext_wall_material), get_mode(ext_wall_material), ext_wall_material),
    roof_material = ifelse(is.na(roof_material), get_mode(roof_material), roof_material),
    basement_type = ifelse(is.na(basement_type), get_mode(basement_type), basement_type),
    basement_finish = ifelse(is.na(basement_finish), get_mode(basement_finish), basement_finish),
    central_heating = ifelse(is.na(central_heating), get_mode(central_heating), central_heating),
    central_air_conditioning = ifelse(is.na(central_air_conditioning), get_mode(central_air_conditioning), central_air_conditioning),
    number_fireplaces = ifelse(is.na(number_fireplaces), median(number_fireplaces, na.rm = TRUE), number_fireplaces),
    attic_type = ifelse(is.na(attic_type), get_mode(attic_type), attic_type),
    usage_type = ifelse(is.na(usage_type), get_mode(usage_type), usage_type),
    flood_risk_factor = ifelse(is.na(flood_risk_factor), get_mode(flood_risk_factor), flood_risk_factor),
    midincome = ifelse(is.na(midincome), median(midincome, na.rm = TRUE), midincome)
  )


if (any(is.na(predict_data$roof_material))) {
  # Calculate the mode of roof_material from non-NA values of the entire dataset
  overall_mode_roof_material <- get_mode(predict_data$roof_material)
  # Apply this mode to fill remaining NAs
  predict_data$roof_material[is.na(predict_data$roof_material)] <- overall_mode_roof_material
}

if (any(is.na(predict_data$ext_wall_material))) {
  # Calculate the mode of ext_wall_material from non-NA values of the entire dataset
  overall_mode_ext_wall_material <- get_mode(predict_data$ext_wall_material)
  # Apply this mode to fill remaining NAs
  predict_data$ext_wall_material[is.na(predict_data$ext_wall_material)] <- overall_mode_ext_wall_material
}


## 2.3 Winsorize numeric variables
numeric_vars <- c("land_area_sqft", "property_age", "number_rooms", "number_beds", "number_fireplaces", "number_full_bath", "number_half_bath", "buidling_area_sqft")

# Apply Winsorizing to each numeric variable
predict_data[numeric_vars] <- lapply(predict_data[numeric_vars], function(x) Winsorize(x, probs = c(0.05, 0.95)))

### 3 Predict Model
# Remove 'pid' from predict_data
predict_data_pid <- predict_data$pid
predict_data <- predict_data[, -which(names(predict_data) == "pid")]

# Train the Random Forest model
rf_model <- randomForest(data$sale_price ~ ., data = data.frame(data), importance = TRUE, ntree = 500)

# Make predictions on the predict_data_encoded
predictions <- predict(rf_model, newdata = predict_data)

# Create a new dataframe with two columns: pid and assessed_value
final_data <- data.frame(pid = predict_data_pid, assessed_value = predictions)

# Round assessed_value to 2 decimal places
final_data$assessed_value <- round(final_data$assessed_value, 2)

# check predicting results
head(final_data)
dim(final_data)

# check the missing values 
missing_values <- colSums(is.na(final_data))
missing_values

# Export as CSV
write.csv(final_data, "assessed_value.csv", row.names = FALSE)
