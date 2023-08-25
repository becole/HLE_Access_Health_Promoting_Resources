# Libraries ---------------------------------------------------------------
setwd("~/Dissertation/Working Directory")
library(tidyverse)
library(ggplot2)
library(stringr)
library(caret)
library(rgdal)
library(ggmap)
library(sf)
library(readr)
library(purrr)
library(skimr)
library(corrplot)
library(tidyr)
library(moments)
library(gridExtra)
library(forcats)
library(cowplot)
library(MASS)
library(dplyr)
library(glmnet)
library(car)
library(broom)
library(ggplotify)
library(grid)

# HLE Data - Processing --------------------------------------------------------------
# Load the health life expectancy (HLE) data for both genders
HLE_female <- read_csv("MSOA.Female.2009.2013.csv") 
HLE_male <- read_csv("MSOA.Male.2009.2013.csv") 

# Handle duplicates in HLE_male before joining
HLE_male <- HLE_male %>%
  distinct(`MSOA Codes`, `MSOA Names`, Region, .keep_all = TRUE)

# Join the male and female datasets together using MSOA codes, names and region
HLE_data <- full_join(HLE_female, HLE_male, by = c("MSOA Codes", "MSOA Names", "Region"), suffix = c("_F", "_M")) %>%
  rename(
    msoa_code = 'MSOA Codes', 
    msoa_name = 'MSOA Names', 
    hle_years_female = `HLE (years)_F`, 
    le_years_female = `LE (Years)_F`, 
    hle_years_male = `HLE (years)_M`, 
    le_years_male = `LE (Years)_M`) %>%
  distinct(msoa_code, .keep_all = TRUE)

rm(HLE_female, HLE_male)


# Calculate the mean values
mean_hle_female <- mean(HLE_data$hle_years_female, na.rm = TRUE)
mean_le_female <- mean(HLE_data$le_years_female, na.rm = TRUE)
mean_hle_male <- mean(HLE_data$hle_years_male, na.rm = TRUE)
mean_le_male <- mean(HLE_data$le_years_male, na.rm = TRUE)

# Replace NA values with the mean for each of the specified columns
HLE_data$hle_years_female[is.na(HLE_data$hle_years_female)] <- mean_hle_female
HLE_data$le_years_female[is.na(HLE_data$le_years_female)] <- mean_le_female
HLE_data$hle_years_male[is.na(HLE_data$hle_years_male)] <- mean_hle_male
HLE_data$le_years_male[is.na(HLE_data$le_years_male)] <- mean_le_male

HLE_data <- HLE_data %>%
  dplyr::select(msoa_code, msoa_name, Region, hle_years_female, hle_years_male, le_years_female, le_years_male)

rm(mean_hle_female, mean_hle_male, mean_le_female, mean_le_male)

# HLE Data - EDA ----------------------------------------------------------

# Define the rename map
rename_map <- c("hle_years_female" = "Female HLE (years)",
                "le_years_female" = "Female LE (years)",
                "hle_years_male" = "Male HLE (years)",
                "le_years_male" = "Male LE (years)"
)

# Check summary statistics and histogram for numeric variables
num_vars <- HLE_data %>% 
  dplyr::select(where(is.numeric))

# Summary statistics
summary_stats <- summary(num_vars)

# Convert to data.frame
summary_stats_df <- as.data.frame(summary_stats)

# Save summary statistics to table
if (!dir.exists("tables")) {
  dir.create("tables")
}

write.csv(summary_stats_df, file = "tables/summary_stats.csv")

# Histogram for numeric variables
hist_plot_hle <- num_vars %>%
  gather(key = "Variable", value = "Value") %>%
  mutate(Variable = factor(Variable, levels = names(rename_map), labels = rename_map)) %>% # replace variable names with chart titles
  ggplot(aes(x = Value)) +
  geom_histogram(bins = 50, color = 'black', fill = viridis(4)[2]) +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(x = "Age", y = "Frequency", title = "Frequency of Outcomes by MSOA") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16), legend.position = "none")

print(hist_plot_hle)

# Boxplots
boxplot_hle <- HLE_data %>%
  gather(variable, value, -msoa_code, -msoa_name, -Region) %>% 
  mutate(variable = factor(variable, levels = names(rename_map), labels = rename_map)) %>%
  ggplot(aes(x = Region, y = value, fill = Region)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free", labeller = label_value) +  # Use `label_value` instead of `label_parsed`
  scale_fill_viridis_d(option = "D") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.45), text = element_text(size = 12), legend.position = "none",
        axis.text.x = element_text(angle = 75, hjust = 1)) +  # Angle the x-axis labels
  scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +  # Wrap long labels
  labs(x = "Region", y = "Value", title = "Spread of Outcomes by Region")
print(boxplot_hle)

# Scatterplots with density contours
scatter_plot_hle <- ggplot(HLE_data, aes(x = hle_years_female, y = hle_years_male)) +
  geom_point(aes(color = Region), alpha = 0.6) +
  scale_color_viridis_d(option = "D") +
  geom_density_2d(data = HLE_data, aes(x = hle_years_female, y = hle_years_male, color = Region), show.legend = FALSE) +
  theme_minimal() +
  labs(x = "HLE Years Female", y = "HLE Years Male", title = "Scatterplot of HLE Years by Gender") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16), legend.position = "bottom")

print(scatter_plot_hle)

scatter_plot_le <- ggplot(HLE_data, aes(x = le_years_female, y = le_years_male)) +
  geom_point(aes(color = Region), alpha = 0.6) +
  scale_color_viridis_d(option = "D") +
  geom_density_2d(data = HLE_data, aes(x = le_years_female, y = le_years_male, color = Region), show.legend = FALSE) +
  theme_minimal() +
  labs(x = "LE Years Female", y = "LE Years Male", title = "Scatterplot of LE Years by Gender") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16), legend.position = "bottom")

print(scatter_plot_le)

# Geospatial mapping

# Load shapefile
msoa_shp <- st_read("MSOA_2011_EW_BFE_V2.shp")
msoa_shp <- msoa_shp %>% filter(str_detect(MSOA11CD, "^E"))

# Merge shapefile with HLE_data
HLE_data <- HLE_data %>%
  full_join(msoa_shp, by = c("msoa_code" = "MSOA11CD", "msoa_name"="MSOA11NM"))

rm(msoa_shp)

# Create a list of unique regions
regions <- unique(HLE_data$Region)

for(region in regions) {
  HLE_data_region <- HLE_data %>% filter(Region == !!region)
  
  for(var in names(rename_map)) {
    geospatial_plot <- ggplot() +
      geom_sf(data = HLE_data_region, aes(geometry = geometry, fill = !!sym(var)), color = "black", size = 0.1) +
      scale_fill_viridis_c(option = "D") +
      theme_minimal() +
      labs(x = "Longitude", y = "Latitude", title = paste0("Geospatial Distribution of ", rename_map[var], " in ", region)) +
      theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16), legend.position = "bottom")
    
    print(geospatial_plot)
    
    # Save the geospatial plot
    if (!dir.exists("visualizations")) {
      dir.create("visualizations")
    }
    
    ggsave(filename = paste0("visualizations/geospatial_", var, "_", region, ".png"), plot = geospatial_plot, width = 11, height = 8, dpi = 300)
  }}

# Save histogram
ggsave(filename = "visualizations/histogram.png", plot = hist_plot_hle, width = 10, height = 10, dpi = 300)

# Save boxplot
ggsave(filename = "visualizations/boxplot.png", plot = boxplot_hle, width = 10, height = 10, dpi = 300)

# Save scatter plots
ggsave(filename = "visualizations/scatter_plot_hle.png", plot = scatter_plot_hle, width = 10, height = 10, dpi = 300)
ggsave(filename = "visualizations/scatter_plot_le.png", plot = scatter_plot_le, width = 10, height = 10, dpi = 300)

# Remove temporary variables to save memory
rm(num_vars, HLE_data_region, hist_plot_hle, rename_map, boxplot_hle, scatter_plot_hle, scatter_plot_le, geospatial_plot, msoa_shp, msoa_shp_region, region, regions, var, summary_stats_df, missing_msoa, col, mean_hle_female, mean_hle_male, mean_le_female, mean_hle_male, mean_le_male, missing_cols, missing_msoas, summary_stats)

saveRDS(HLE_data, "HLE_data.rds")

# Access Data - Processing -------------------------------------------

# Define a function to read and summarise NA values
read_and_summarise <- function(filepath) {
  df <- read_csv(filepath)
  summarise(df, across(everything(), ~sum(is.na(.))))
  return(df)
}

# Load and summarise data files
data_files <- list("access_supermarkets_pt.csv", "access_cities_pt.csv", "access_employment_pt.csv", 
                   "access_gp_pt.csv", "access_hospital_pt.csv", "access_school_pt.csv")

# Apply the read_and_summarise function to each file and store the results in a list
data_list <- lapply(data_files, read_and_summarise)

# Join the data frames in the list into one data frame
access_data <- Reduce(function(d1, d2) left_join(d1, d2, by = c("geo_code", "geo_label")), data_list)

# Filter out non-English LSOAs
access_data <- access_data %>%
  filter(str_detect(geo_code, "^E"))

# Create lists of variable names for sum and mean calculations
variables_repeats = c("supermarket_pct_15", "supermarket_pct_30", "supermarket_pct_45", 
                   "supermarket_pct_60", "supermarket_pct_75", "supermarket_pct_90", 
                   "supermarket_pct_105", "supermarket_pct_120", "nearest_supermarket", 
                   "nearest_main_bua", "nearest_sub_bua", "employment_pct_15", 
                   "employment_pct_30", "employment_pct_45", "employment_pct_60", 
                   "employment_pct_75", "employment_pct_90", "employment_pct_105", 
                   "employment_pct_120", "gp_pct_15", "gp_pct_30", 
                   "gp_pct_45", "gp_pct_60", "gp_pct_75", 
                   "gp_pct_90", "gp_pct_105", "gp_pct_120", 
                   "nearest_gp", "hospitals_pct_15", "hospitals_pct_30", 
                   "hospitals_pct_45", "hospitals_pct_60", "hospitals_pct_75", 
                   "hospitals_pct_90", "hospitals_pct_105", "hospitals_pct_120", 
                   "nearest_hosp", "school_pct_primary_15", "school_pct_primary_30", 
                   "school_pct_primary_45", "school_pct_primary_60", "school_pct_primary_75", 
                   "school_pct_primary_90", "school_pct_primary_105", "school_pct_primary_120", 
                   "school_pct_secondary_15","school_pct_secondary_30", "school_pct_secondary_45", "school_pct_secondary_60", 
                   "school_pct_secondary_75", "school_pct_secondary_90", "school_pct_secondary_105", 
                   "school_pct_secondary_120", "nearest_primary", "nearest_secondary")

# Remove the unneeded mean variables
access_data <- access_data %>%
  dplyr::select(-all_of(variables_repeats)) 

# Clean up the workspace by removing variables no longer needed
rm(data_files, data_list, read_and_summarise, variables_repeats)

# Load the lookup table for 2011 LSOA to MSOA
lsoa_msoa_lookup_2011_all <- read_csv("LSOA11_MSOA11.csv")

# Select only the LSOA and MSOA columns from the lookup table
lsoa_msoa_lookup_2011 <- lsoa_msoa_lookup_2011_all %>%
  dplyr::select(LSOA11CD, MSOA11CD) 

# Remove the original lookup table from the workspace
rm(lsoa_msoa_lookup_2011_all)

# Merge the access data with the lookup table to map 2011 LSOAs to MSOAs
access_data_english_msoa <- access_data %>%
  left_join(lsoa_msoa_lookup_2011, by = c("geo_code" = "LSOA11CD"))

# Remove the updated access data and lookup table from the workspace
rm(lsoa_msoa_lookup_2011)

# Variables for sum
variables = c("supermarket_15", "supermarket_30", "supermarket_45", "supermarket_60", 
                  "supermarket_75", "supermarket_90", "supermarket_105", "supermarket_120", 
                  "employment_15", "employment_30", "employment_45", "employment_60", 
                  "employment_75", "employment_90", "employment_105", "employment_120", 
                  "gp_number_15", "gp_number_30", "gp_number_45", "gp_number_60", 
                  "gp_number_75", "gp_number_90", "gp_number_105", "gp_number_120",
                  "hospitals_15", "hospitals_30", "hospitals_45", "hospitals_60", 
                  "hospitals_75", "hospitals_90", "hospitals_105", "hospitals_120",
                  "school_primary_15", "school_primary_30", "school_primary_45", "school_primary_60", 
                  "school_primary_75", "school_primary_90", "school_primary_105", "school_primary_120",
                  "school_secondary_15", "school_secondary_30","school_secondary_45", "school_secondary_60", "school_secondary_75", 
                  "school_secondary_90", "school_secondary_105", "school_secondary_120")

# Group the data by MSOA and summarize
access_data_complete <- access_data_english_msoa %>%
  group_by(MSOA11CD) %>%
  summarise(
    across(all_of(variables), .fns = sum, na.rm = TRUE))

# Scale the variables
access_data_complete <- access_data_complete %>%
  mutate(across(all_of(variables), scale))

# Remove the English MSOA access data and the lists of variables from the workspace
rm(access_data_english_msoa, variables, access_data)

# Access Data - EDA -------------------------------------------------------

# Define the access variables 
access_vars = c("supermarket_15", "supermarket_30", "supermarket_45", "supermarket_60", 
                "supermarket_75", "supermarket_90", "supermarket_105", "supermarket_120", 
                "employment_15", "employment_30", "employment_45", "employment_60", 
                "employment_75", "employment_90", "employment_105", "employment_120", 
                "gp_number_15", "gp_number_30", "gp_number_45", "gp_number_60", 
                "gp_number_75", "gp_number_90", "gp_number_105", "gp_number_120",
                "hospitals_15", "hospitals_30", "hospitals_45", "hospitals_60", 
                "hospitals_75", "hospitals_90", "hospitals_105", "hospitals_120",
                "school_primary_15", "school_primary_30", "school_primary_45", "school_primary_60", 
                "school_primary_75", "school_primary_90", "school_primary_105", "school_primary_120",
                "school_secondary_15", "school_secondary_30","school_secondary_45", "school_secondary_60", "school_secondary_75", 
                "school_secondary_90", "school_secondary_105", "school_secondary_120")

# Define the access variables 
access_vars_base <- c("supermarket", "employment", "gp_number", "hospitals", "school_primary", "school_secondary")

# Define the time cuts
time_cuts <- seq(15, 120, by = 15)

# Variables for 30 minute time cut
access_vars_30 <- paste0(access_vars_base, "_30")

# Initialize data frame for summary statistics
summary_stats <- data.frame()

# List to hold individual histogram plots for 30 minute time cut
histogram_plots_list <- list()

# Generate individual histograms and summary stats for 30 minute time cut
for(access_var in access_vars_30) {
  
  if(access_var %in% names(access_data_complete)) {
    
    # Compute summary statistics
    data_subset <- as.numeric(access_data_complete[[access_var]])

    # Generate histogram
    histogram_plot <- ggplot(access_data_complete, aes_string(x = access_var)) +
      geom_histogram(fill = scales::viridis_pal()(1), bins = 30, alpha = 0.7) +
      theme_minimal() +
      labs(title = access_vars , y = "Frequency", x = "Value")
    
    # Add the histogram to the list
    histogram_plots_list[[access_var]] <- histogram_plot
    
    # Compute summary statistics for the 30 minute time cut
    summary_stats_row <- data.frame(
      "Min" = min(data_subset, na.rm = TRUE),
      "1st Qu" = quantile(data_subset, 0.25, na.rm = TRUE),
      "Median" = median(data_subset, na.rm = TRUE),
      "Mean" = mean(data_subset, na.rm = TRUE),
      "3rd Qu" = quantile(data_subset, 0.75, na.rm = TRUE),
      "Max" = max(data_subset, na.rm = TRUE)
    )
    summary_stats_row$Variable <- access_var
    
    # Add to the summary stats data frame
    summary_stats <- rbind(summary_stats, summary_stats_row)
  }
}

# Combine the individual histograms
combined_plots_grob <- arrangeGrob(grobs = histogram_plots_list, ncol = 2, top = textGrob("Histogram of Access Variables (30 Min)", gp = gpar(fontsize = 16)))

# Save the combined plots
ggsave(filename = "visualizations/multi_hist_30.png", plot = combined_plots_grob, width = 10, height = 8)

# Save summary stats to 'tables' folder
write.csv(summary_stats, "tables/summary_stats_30.csv", row.names = TRUE)

# Correlation across all time cuts
for(time_cut in time_cuts) {
  
  # Create a matrix for the correlations at the current time cut
  cor_matrix <- cor(access_data_complete[, paste0(access_vars_base, "_", time_cut)])
  
  # Write the matrix to a .csv file
  tryCatch({
    write.csv(cor_matrix, file = paste0("tables/correlation_matrix_", time_cut, ".csv"))
    print(paste0("Correlation matrix saved for time cut ", time_cut))
  }, error = function(e) {
    print(paste("Error saving correlation matrix for time cut", time_cut, ":", e$message))
  })
}

# Cleanup
rm(cor_matrix, histogram_plots_list, combined_plots_grob, histogram_plot, access_vars, access_vars_base, summary_stats, summary_stats_row, access_var, access_vars_30, time_cut, time_cuts, data_subset)

# Census Data - Processing -------------------------------------------------------------
process_data <- function(data, total_col){
  # If total_col is not NULL
  if (!is.null(total_col)) {
    # Check if total_col exists in the data
    if (!total_col %in% names(data)) {
      stop(paste("Column", total_col, "does not exist in the data."))
    }
    
    # Exclude columns that shouldn't be processed
    exclude_cols <- c("geography", "geography code", total_col)
    
    # Get the categories excluding the specified columns
    categories <- setdiff(names(data), exclude_cols)
    
    # For each category, create a new column 'prop_' with the proportion
    for (cat in categories){
      # Diagnostic print statements
      print(paste("Processing column:", cat))
      if (any(is.na(data[[cat]])) || any(is.na(data[[total_col]]))) {
        print("Warning: NA values detected.")
      }
      if (any(data[[total_col]] == 0)) {
        print("Warning: Zero values detected in total column.")
      }
      
      data[[paste0("prop_", cat)]] <- data[[cat]] / data[[total_col]]
    }
    
    # Remove original categories columns
    data <- data[, !names(data) %in% categories]
  }
  return(data)
}

# Original list of files with their respective total column
files <- list(
  "Census_Accom_Type_2011.csv" = "Total_Accom_Type",
  "Census_Age_2011.csv" = "Total_Age",
  "Census_Car_Access_2011.csv" = "Total_Car_Access",
  "Census_Disability_2011.csv" = "Total_Disability",
  "Census_Edu_2011.csv" = "Total_Edu",
  "Census_employment_type_2011.csv" = "Total_Employment", 
  "Census_English_Prof_2011.csv" = "Total_English_Prof",
  "Census_Ethnicity_2011.csv" = "Total_Ethnicity",
  "Census_Migrant_2011.csv" = "Total_Migrant"
)

# Initialize an empty variable to store the processed data
census_data <- NULL

# Loop over each file
for (file in names(files)) {
  # Read the file
  data <- read_csv(file)
  # Process the data using the process_data function
  data <- process_data(data, files[[file]])
  
  # If census_data is NULL, assign data to it, else join the new data with census_data
  if (is.null(census_data)) {
    census_data <- data
  } else {
    data$date <- NULL
    census_data <- full_join(census_data, data, by = c("geography" = "geography", "geography code" = "geography code"))
  }
}

# Filter out rows with geography code not starting with 'E'
if(!is.null(census_data)){
  census_data <- filter(census_data, str_detect(`geography code`, "^E"))
} else {
  print("census_data is NULL. Check if the files were read correctly.")
}

total_columns_to_remove <- c(
  "Total_Accom_Type",
  "Total_Age",
  "Total_Car_Access",
  "Total_Disability",
  "Total_Edu",
  "Total_Employment",
  "Total_English_Prof",
  "Total_Ethnicity",
  "Total_Migrant"
)

# Correct the column renaming
census_data <- census_data %>%
  dplyr::select(-all_of(total_columns_to_remove))
names(census_data) <- gsub("[^[:alnum:]]", "_", names(census_data))

# Remove the temporary variables to free up memory
rm(data, files, file, process_data, total_columns_to_remove)

# Census Data - EDA -------------------------------------------------------

# Define category variables

# Accommodation Type
accommodation_type_vars <- c(
  "prop_Dwelling_Type__Unshared_dwelling__Whole_house_or_bungalow__Detached__measures__Value",
  "prop_Dwelling_Type__Unshared_dwelling__Whole_house_or_bungalow__Semi_detached__measures__Value",
  "prop_Dwelling_Type__Unshared_dwelling__Whole_house_or_bungalow__Terraced__including_end_terrace___measures__Value",
  "prop_Dwelling_Type__Unshared_dwelling__Flat__maisonette_or_apartment__Purpose_built_block_of_flats_or_tenement__measures__Value",
  "prop_Dwelling_Type__Unshared_dwelling__Flat__maisonette_or_apartment__Part_of_a_converted_or_shared_house__including_bed_sits___measures__Value",
  "prop_Dwelling_Type__Unshared_dwelling__Flat__maisonette_or_apartment__In_commercial_building__measures__Value",
  "prop_Dwelling_Type__Unshared_dwelling__Caravan_or_other_mobile_or_temporary_structure__measures__Value"
)

# Age
age_vars <- c(
  "prop_Age__Age_under_1__measures__Value",
  paste0("prop_Age__Age_", 1:99, "__measures__Value"),
  "prop_Age__Age_100_and_over__measures__Value"
)

# Number of cars or vans
car_van_vars <- c(
  "prop_Cars__No_cars_or_vans_in_household__measures__Value",
  "prop_Cars__1_car_or_van_in_household__measures__Value",
  "prop_Cars__2_cars_or_vans_in_household__measures__Value",
  "prop_Cars__3_cars_or_vans_in_household__measures__Value",
  "prop_Cars__4_or_more_cars_or_vans_in_household__measures__Value",
  "prop_Cars__sum_of_All_cars_or_vans_in_the_area__measures__Value"
)

# Disability
disability_vars <- c(
  "prop_Disability__Day_to_day_activities_limited_a_lot__measures__Value",
  "prop_Disability__Day_to_day_activities_limited_a_little__measures__Value",
  "prop_Disability__Day_to_day_activities_not_limited__measures__Value"
)

# Highest level of qualification
qualification_vars <- c(
  "prop_Qualification__No_qualifications__measures__Value",
  "prop_Qualification__Level_1_qualifications__measures__Value",
  "prop_Qualification__Level_2_qualifications__measures__Value",
  "prop_Qualification__Apprenticeship__measures__Value",
  "prop_Qualification__Level_3_qualifications__measures__Value",
  "prop_Qualification__Level_4_qualifications_and_above__measures__Value",
  "prop_Qualification__Other_qualifications__measures__Value"
)

# Occupation (current)
occupation_vars <- c(
  "prop_Occupation__1__Managers__directors_and_senior_officials__measures__Value",
  "prop_Occupation__2__Professional_occupations__measures__Value",
  "prop_Occupation__3__Associate_professional_and_technical_occupations__measures__Value",
  "prop_Occupation__4__Administrative_and_secretarial_occupations__measures__Value",
  "prop_Occupation__5__Skilled_trades_occupations__measures__Value",
  "prop_Occupation__6__Caring__leisure_and_other_service_occupations__measures__Value",
  "prop_Occupation__7__Sales_and_customer_service_occupations__measures__Value",
  "prop_Occupation__8__Process_plant_and_machine_operatives__measures__Value",
  "prop_Occupation__9__Elementary_occupations__measures__Value"
)

# Proficiency in English language
proficiency_vars <- c(
  "prop_Proficiency_in_English__Main_language_is_English__English_or_Welsh_in_Wales___measures__Value",
  "prop_Proficiency_in_English__Main_language_is_not_English__English_or_Welsh_in_Wales___Can_speak_English_very_well__measures__Value",
  "prop_Proficiency_in_English__Main_language_is_not_English__English_or_Welsh_in_Wales___Can_speak_English_well__measures__Value",
  "prop_Proficiency_in_English__Main_language_is_not_English__English_or_Welsh_in_Wales___Cannot_speak_English_well__measures__Value",
  "prop_Proficiency_in_English__Main_language_is_not_English__English_or_Welsh_in_Wales___Cannot_speak_English__measures__Value"
)

# Ethnic Group
ethnic_group_vars <- c(
  "prop_Ethnic_Group__White__measures__Value",
  "prop_Ethnic_Group__White__English_Welsh_Scottish_Northern_Irish_British__measures__Value",
  "prop_Ethnic_Group__White__Irish__measures__Value",
  "prop_Ethnic_Group__White__Gypsy_or_Irish_Traveller__measures__Value",
  "prop_Ethnic_Group__White__Other_White__measures__Value",
  "prop_Ethnic_Group__Mixed__measures__Value",
  "prop_Ethnic_Group__Mixed_multiple_ethnic_group__White_and_Black_Caribbean__measures__Value",
  "prop_Ethnic_Group__Mixed_multiple_ethnic_group__White_and_Black_African__measures__Value",
  "prop_Ethnic_Group__Mixed_multiple_ethnic_group__White_and_Asian__measures__Value",
  "prop_Ethnic_Group__Mixed_multiple_ethnic_group__Other_Mixed__measures__Value",
  "prop_Ethnic_Group__Asian__measures__Value",
  "prop_Ethnic_Group__Asian_Asian_British__Indian__measures__Value",
  "prop_Ethnic_Group__Asian_Asian_British__Pakistani__measures__Value",
  "prop_Ethnic_Group__Asian_Asian_British__Bangladeshi__measures__Value",
  "prop_Ethnic_Group__Asian_Asian_British__Chinese__measures__Value",
  "prop_Ethnic_Group__Asian_Asian_British__Other_Asian__measures__Value",
  "prop_Ethnic_Group__Black__measures__Value",
  "prop_Ethnic_Group__Black_African_Caribbean_Black_British__African__measures__Value",
  "prop_Ethnic_Group__Black_African_Caribbean_Black_British__Caribbean__measures__Value",
  "prop_Ethnic_Group__Black_African_Caribbean_Black_British__Other_Black__measures__Value",
  "prop_Ethnic_Group__Other__measures__Value",
  "prop_Ethnic_Group__Other_ethnic_group__Arab__measures__Value",
  "prop_Ethnic_Group__Other_ethnic_group__Any_other_ethnic_group__measures__Value"
)

# Migration
migration_vars <- c(
  "prop_Migration__Lived_at_same_address_one_year_ago__measures__Value",
  "prop_Migration__Migrants__Total__measures__Value",
  "prop_Migration__Migrants__Moved_within_the_area__measures__Value",
  "prop_Migration__Migrants__Moved_into_the_area_from_within_the_UK__measures__Value",
  "prop_Migration__Migrants__Moved_into_the_area_from_outside_the_UK__measures__Value",
  "prop_Migration__Moved_out_of_the_area__measures__Value"
)

# Combine all categories into one vector
all_vars <- c(accommodation_type_vars, age_vars, car_van_vars, disability_vars, qualification_vars,
              occupation_vars, proficiency_vars, ethnic_group_vars, migration_vars)

# Select relevant columns from the data
selected_data <- census_data %>% dplyr::select(all_vars)

# Assuming you've already skimmed the data
summary_stats <- selected_data %>% skim()

# Convert the skim result to a data frame for saving
summary_stats_df <- summary_stats %>% as.data.frame()

# Save the data frame to a CSV file
write.csv(summary_stats_df, file = paste0("tables/summary_statistics_census.csv"), row.names = FALSE)

# Remove temporary variables
rm(list=setdiff(ls(),c("access_data_complete","census_data", "HLE_data")))

# Complete Data - Processing -------------------------------------------------------

# Rename columns in the 'access_data_complete' and 'census_data' for uniformity
access_data <- access_data_complete %>% rename(msoa_code = MSOA11CD)
census_data_complete <- census_data %>% rename(msoa_code = geography_code, msoa_name = geography)

# Perform full joins and rearrange columns
complete_data <- full_join(access_data, HLE_data, by = "msoa_code", "msoa_name") %>%
  full_join(census_data_complete, by = c("msoa_code","msoa_name")) %>%
  dplyr::select(msoa_code, everything())

# Removing duplicate rows if `msoa_code` and `msoa_name` are repeating in the data
complete_data <- distinct(complete_data, msoa_code, .keep_all = TRUE)

# Remove the original datasets to save memory
rm(HLE_data, access_data_complete, census_data, access_data, census_data_complete)

# Clean column names of the merged dataset
names(complete_data) <- gsub("[^[:alnum:]]", "_", names(complete_data))

# Check for special characters in variable names
print(grep("[^[:alnum:]_]", names(complete_data), value = TRUE))

# Count the number of rows with any NA
na_rows_count <- sum(apply(complete_data, 1, function(row) any(is.na(row))))

print(na_rows_count)

rm(na_rows_count)

# Complete Data - EDA -----------------------------------------------------
# Load shapefile
msoa_shp <- st_read("MSOA_2011_EW_BFE_V2.shp") %>%
  filter(str_detect(MSOA11CD, "^E")) %>%
  left_join(complete_data, by = c("MSOA11CD" = "msoa_code"))  # Merge shapefile with complete_data

saveRDS(complete_data, "complete_data.rds")
saveRDS(msoa_shp, "msoa_shp.rds")


# Create a list of unique regions
regions <- unique(complete_data$Region)

# Define time cuts for access variables
time_cuts <- 30

# Define list of base access variables
access_vars_base <- c("supermarket", "employment", "gp_number", "hospitals", "school_primary", "school_secondary")

# Define list of health outcomes
outcome_vars <- c("hle_years_male", "hle_years_female", "le_years_male", "le_years_female")

# Define function for creating bivariate categories
create_bivariate_categories <- function(df, var1, var2) {
  df$cat <- ifelse(df[[var1]] > median(df[[var1]], na.rm = TRUE) & df[[var2]] > median(df[[var2]], na.rm = TRUE), "High-High",
                   ifelse(df[[var1]] <= median(df[[var1]], na.rm = TRUE) & df[[var2]] <= median(df[[var2]], na.rm = TRUE), "Low-Low",
                          ifelse(df[[var1]] > median(df[[var1]], na.rm = TRUE) & df[[var2]] <= median(df[[var2]], na.rm = TRUE), "High-Low",
                                 "Low-High"
                          )
                   )
  )
  df
}

# Initialize directory for visualizations
if (!dir.exists("visualizations")) {
  dir.create("visualizations")
}

# Loop over regions to create a plot for each
for(region in regions) {
  
  msoa_shp_region <- msoa_shp %>% filter(Region == region) # regular == operator used here
  
  # Loop over base access variables and outcome variables
  for(access_var_base in access_vars_base) {
    for(outcome_var in outcome_vars) {
      
      # Loop over time cuts to create access_vars
      for(time_cut in time_cuts) {
        access_var <- paste0(access_var_base, "_", time_cut)
        
        # Make sure the columns exist before trying to create the bivariate_value
        if(access_var %in% colnames(msoa_shp_region) & outcome_var %in% colnames(msoa_shp_region)) {
          
          # Create bivariate categories
          msoa_shp_region <- create_bivariate_categories(msoa_shp_region, access_var, outcome_var)
          
          # Create bivariate map
          bivariate_map <- ggplot() +
            geom_sf(data = msoa_shp_region, aes(fill = cat), color = "black", size = 0.1) +
            scale_fill_viridis_d(option = "D", labels = c("High-High", "Low-Low", "High-Low", "Low-High")) +
            theme_minimal() +
            labs(x = "Longitude", y = "Latitude", fill = "Bivariate category",
                 title = paste0("Bivariate map of ", access_var, " and ", outcome_var, " in ", region)) +
            theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16), legend.position = "bottom")
          
          # Save the plot
          ggsave(filename = paste0("visualizations/bivariate_", access_var_base, "_", outcome_var, "_", region, "_", time_cut, ".png"), 
                 plot = bivariate_map, width = 16, height = 9, dpi = 300)
          
          # Remove bivariate variable to keep data clean
          msoa_shp_region <- msoa_shp_region %>%  dplyr::select(-cat)
        }
      }
    }
  }
}

# Loop over time cuts to create access_vars
for(time_cut in time_cuts) {
  access_vars <- paste0(access_vars_base, "_", time_cut)
  
  # Initialize an empty data frame to store results
  descr_stats <- data.frame()
  color_palette <- viridis(3)
  
  # Loop over specified access and health outcome variables
  for(access_var in access_vars){
    if(access_var %in% names(complete_data)){
      for(outcome_var in outcome_vars){
        if(outcome_var %in% names(complete_data)){
          # ANOVA test
          lm_model <- lm(as.formula(paste0(outcome_var, "~", access_var)), data = complete_data)
          anova_result <- anova(lm_model)
          print(paste0("ANOVA results for ", access_var, " and ", outcome_var))
          print(anova_result)
          
          # Save ANOVA result to the tables folder
          write.csv(anova_result, paste0("tables/ANOVA_", access_var, "_", outcome_var, ".csv"))
          
          rm(lm_model, anova_result)  # Remove temporary variables
        }
      }
    }
  }
}
  
for(time_cut in time_cuts) {
  # Extracting variables related to the specific time cut
  selected_vars <- c(grep(paste0("_", time_cut, "$"), names(complete_data), value = TRUE), outcome_vars)
  selected_data <- complete_data[selected_vars]
  correlation_matrix <- selected_data %>% cor(method = "spearman", use = "pairwise.complete.obs")
  
  # Save the plot
  png(filename = paste0("visualizations/corr_plot_", time_cut, ".png"), width = 10, height = 10, units = "in", res = 300)
  corrplot(correlation_matrix, method = "color", col = viridis::viridis(200), tl.col = "black")
  dev.off()
}

# Remove temporary variables
rm(selected_data, p)

# Loop through time cuts and outcome variables
for (time_cut in time_cuts) {
  for (outcome_var in outcome_vars) {
    # Create a data frame that includes all the access variables for one time cut and the selected outcome variable
    plot_data <- complete_data %>%
      dplyr::select(all_of(c(paste0(access_vars_base, "_", time_cut), outcome_var))) %>%
      pivot_longer(cols = all_of(paste0(access_vars_base, "_", time_cut)), names_to = "access_var", values_to = "access_value")
    
    # Create scatter plot
    scatter_plot <- ggplot(plot_data, aes(x = access_value, y = !!sym(outcome_var), color = access_var)) +
      geom_point(alpha = 0.6) +
      scale_color_viridis_d(option = "D") +
      theme_minimal() +
      labs(x = paste0("Log Transformed Access Variables at ", time_cut, " Minute Time Cut"), y = outcome_var, title = paste0("Scatterplot of ", outcome_var, " at ", time_cut, " Minute Time Cut")) +
      theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16), legend.position = "bottom")
    
    # Save individual plot
    ggsave(paste0("visualizations/scatter_plot_", time_cut, "_", outcome_var, ".png"), scatter_plot, width = 15, height = 10)
  }
}


# Remove temporary variables
rm(list=setdiff(ls(), "complete_data"))

# Model - Data Setup ---------------------------------------------------------------
# Define Control Variable Categories
# Accommodation Type
accommodation_type_vars <- c(
  "prop_Dwelling_Type__Unshared_dwelling__Whole_house_or_bungalow__Detached__measures__Value",
  "prop_Dwelling_Type__Unshared_dwelling__Whole_house_or_bungalow__Semi_detached__measures__Value",
  "prop_Dwelling_Type__Unshared_dwelling__Whole_house_or_bungalow__Terraced__including_end_terrace___measures__Value",
  "prop_Dwelling_Type__Unshared_dwelling__Flat__maisonette_or_apartment__Purpose_built_block_of_flats_or_tenement__measures__Value",
  "prop_Dwelling_Type__Unshared_dwelling__Flat__maisonette_or_apartment__Part_of_a_converted_or_shared_house__including_bed_sits___measures__Value",
  "prop_Dwelling_Type__Unshared_dwelling__Flat__maisonette_or_apartment__In_commercial_building__measures__Value",
  "prop_Dwelling_Type__Unshared_dwelling__Caravan_or_other_mobile_or_temporary_structure__measures__Value"
)

# Age
age_vars <- c(
  "prop_Age__Age_under_1__measures__Value",
  paste0("prop_Age__Age_", 1:99, "__measures__Value"),
  "prop_Age__Age_100_and_over__measures__Value"
)

# Number of cars or vans
car_van_vars <- c(
  "prop_Cars__No_cars_or_vans_in_household__measures__Value",
  "prop_Cars__1_car_or_van_in_household__measures__Value",
  "prop_Cars__2_cars_or_vans_in_household__measures__Value",
  "prop_Cars__3_cars_or_vans_in_household__measures__Value",
  "prop_Cars__4_or_more_cars_or_vans_in_household__measures__Value",
  "prop_Cars__sum_of_All_cars_or_vans_in_the_area__measures__Value"
)

# Disability
disability_vars <- c(
  "prop_Disability__Day_to_day_activities_limited_a_lot__measures__Value",
  "prop_Disability__Day_to_day_activities_limited_a_little__measures__Value",
  "prop_Disability__Day_to_day_activities_not_limited__measures__Value"
)

# Highest level of qualification
qualification_vars <- c(
  "prop_Qualification__No_qualifications__measures__Value",
  "prop_Qualification__Level_1_qualifications__measures__Value",
  "prop_Qualification__Level_2_qualifications__measures__Value",
  "prop_Qualification__Apprenticeship__measures__Value",
  "prop_Qualification__Level_3_qualifications__measures__Value",
  "prop_Qualification__Level_4_qualifications_and_above__measures__Value",
  "prop_Qualification__Other_qualifications__measures__Value"
)

# Occupation (current)
occupation_vars <- c(
  "prop_Occupation__1__Managers__directors_and_senior_officials__measures__Value",
  "prop_Occupation__2__Professional_occupations__measures__Value",
  "prop_Occupation__3__Associate_professional_and_technical_occupations__measures__Value",
  "prop_Occupation__4__Administrative_and_secretarial_occupations__measures__Value",
  "prop_Occupation__5__Skilled_trades_occupations__measures__Value",
  "prop_Occupation__6__Caring__leisure_and_other_service_occupations__measures__Value",
  "prop_Occupation__7__Sales_and_customer_service_occupations__measures__Value",
  "prop_Occupation__8__Process_plant_and_machine_operatives__measures__Value",
  "prop_Occupation__9__Elementary_occupations__measures__Value"
)

# Proficiency in English language
proficiency_vars <- c(
  "prop_Proficiency_in_English__Main_language_is_English__English_or_Welsh_in_Wales___measures__Value",
  "prop_Proficiency_in_English__Main_language_is_not_English__English_or_Welsh_in_Wales___Can_speak_English_very_well__measures__Value",
  "prop_Proficiency_in_English__Main_language_is_not_English__English_or_Welsh_in_Wales___Can_speak_English_well__measures__Value",
  "prop_Proficiency_in_English__Main_language_is_not_English__English_or_Welsh_in_Wales___Cannot_speak_English_well__measures__Value",
  "prop_Proficiency_in_English__Main_language_is_not_English__English_or_Welsh_in_Wales___Cannot_speak_English__measures__Value"
)

# Ethnic Group
ethnic_group_vars <- c(
  "prop_Ethnic_Group__White__measures__Value",
  "prop_Ethnic_Group__White__English_Welsh_Scottish_Northern_Irish_British__measures__Value",
  "prop_Ethnic_Group__White__Irish__measures__Value",
  "prop_Ethnic_Group__White__Gypsy_or_Irish_Traveller__measures__Value",
  "prop_Ethnic_Group__White__Other_White__measures__Value",
  "prop_Ethnic_Group__Mixed__measures__Value",
  "prop_Ethnic_Group__Mixed_multiple_ethnic_group__White_and_Black_Caribbean__measures__Value",
  "prop_Ethnic_Group__Mixed_multiple_ethnic_group__White_and_Black_African__measures__Value",
  "prop_Ethnic_Group__Mixed_multiple_ethnic_group__White_and_Asian__measures__Value",
  "prop_Ethnic_Group__Mixed_multiple_ethnic_group__Other_Mixed__measures__Value",
  "prop_Ethnic_Group__Asian__measures__Value",
  "prop_Ethnic_Group__Asian_Asian_British__Indian__measures__Value",
  "prop_Ethnic_Group__Asian_Asian_British__Pakistani__measures__Value",
  "prop_Ethnic_Group__Asian_Asian_British__Bangladeshi__measures__Value",
  "prop_Ethnic_Group__Asian_Asian_British__Chinese__measures__Value",
  "prop_Ethnic_Group__Asian_Asian_British__Other_Asian__measures__Value",
  "prop_Ethnic_Group__Black__measures__Value",
  "prop_Ethnic_Group__Black_African_Caribbean_Black_British__African__measures__Value",
  "prop_Ethnic_Group__Black_African_Caribbean_Black_British__Caribbean__measures__Value",
  "prop_Ethnic_Group__Black_African_Caribbean_Black_British__Other_Black__measures__Value",
  "prop_Ethnic_Group__Other__measures__Value",
  "prop_Ethnic_Group__Other_ethnic_group__Arab__measures__Value",
  "prop_Ethnic_Group__Other_ethnic_group__Any_other_ethnic_group__measures__Value"
)

# Migration
migration_vars <- c(
  "prop_Migration__Lived_at_same_address_one_year_ago__measures__Value",
  "prop_Migration__Migrants__Total__measures__Value",
  "prop_Migration__Migrants__Moved_within_the_area__measures__Value",
  "prop_Migration__Migrants__Moved_into_the_area_from_within_the_UK__measures__Value",
  "prop_Migration__Migrants__Moved_into_the_area_from_outside_the_UK__measures__Value",
  "prop_Migration__Moved_out_of_the_area__measures__Value"
)


# Define outcome variables and control variables
outcomes <- c("hle_years_male", "hle_years_female", "le_years_male", "le_years_female")
control_vars <- c(accommodation_type_vars, age_vars, car_van_vars, disability_vars, qualification_vars,
                  occupation_vars, proficiency_vars, ethnic_group_vars)

# Define Explanatory Variables (Access Variables for 30min)
access_types <- c("supermarket", "employment", "gp_number", "hospitals", "school_primary", "school_secondary")

# Model - Processing ------------------------------------------------------

# Define Explanatory Variables for the 30-minute time cut
access_vars <- paste0(access_types, "_30")

# Containers for model results and summaries
linear_models <- list()
linear_coef_tables <- list()
lasso_models <- list()
lasso_coef_tables <- list()

# Modeling for each outcome
for (outcome in outcomes) {
  
  # Linear regression
  model_formula <- as.formula(paste(outcome, "~", paste(c(control_vars, access_vars), collapse = " + ")))
  linear_model <- lm(model_formula, data = complete_data)
  
  model_name <- paste(outcome, "linear", sep = "_")
  linear_models[[model_name]] <- linear_model
  
  coef_df <- summary(linear_model)$coefficients
  coef_df <- coef_df[rownames(coef_df) %in% access_vars, ]
  linear_coef_tables[[model_name]] <- coef_df
  
  write.csv(coef_df, file = paste0("tables/", model_name, "_linear_coefs.csv"), row.names = TRUE)
  
  # Lasso regression
  x <- model.matrix(model_formula, data = complete_data)[, -1]  # remove intercept column
  y <- complete_data[[outcome]]
  lasso_model <- cv.glmnet(x, y, alpha = 1)
  
  model_name <- paste(outcome, "lasso", sep = "_")
  lasso_models[[model_name]] <- lasso_model
  
  # Correct
  lasso_coefs <- as.data.frame(as.matrix(coef(lasso_model, s = lasso_model$lambda.min)))
  names(lasso_coefs) <- c("Coefficient")
  coef_path <- lasso_coefs
  
  lasso_coef_tables[[model_name]] <- coef_path
  
  write.csv(coef_path, file = paste0("tables/", model_name, "_lasso_coefs.csv"), row.names = TRUE)
}

# Model - EDA ---------------------------------------------------------------

# Define directories for plots and tables
plot_dir <- "visualizations"
table_dir <- "tables"

# Dimensions for plots
plot_width <- 8
plot_height <- 6
vi_plot_width <- 10
vi_plot_height <- 8

# Check if directories exist and if not, create them
if (!dir.exists(plot_dir)) dir.create(plot_dir)
if (!dir.exists(table_dir)) dir.create(table_dir)

# Visualization for Linear Models
for (name in names(linear_models)) {
  
  # Create a data frame with fitted values and residuals for plotting
  fitted_and_residuals <- data.frame(
    Fitted = fitted(linear_models[[name]]),
    Residuals = residuals(linear_models[[name]])
  )
  
  # Residual Plot
  plot <- ggplot(fitted_and_residuals, aes(x = Fitted, y = Residuals)) + 
    geom_point(aes(color = Residuals), alpha = 0.5) + 
    scale_color_viridis() + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    labs(title = paste("Residuals for", name), x = "Fitted Values", y = "Residuals")
  ggsave(filename = paste0(plot_dir, "/", name, "_linear_residuals.png"), plot = plot, width = plot_width, height = plot_height)
  
  # Variable Importance for Linear Regression
  coef_df <- as.data.frame(linear_coef_tables[[name]]) # Convert to data frame explicitly
  coef_df$Variable <- rownames(coef_df) # Add a column for the variable names
  plot <- ggplot(coef_df, aes(x = reorder(Variable, Estimate), y = Estimate)) +
    geom_col(aes(fill = Estimate)) +
    scale_fill_viridis() +
    labs(title = paste("Variable Importance (Linear) for", name), x = "Variables", y = "Coefficient Value") +
    coord_flip()
  ggsave(filename = paste0(plot_dir, "/", name, "_linear_variable_importance.png"), plot = plot, width = vi_plot_width, height = vi_plot_height)
}

# Visualization for Lasso Models
for (name in names(lasso_models)) {
  
  # Lasso Coefficient Path
  coef_matrix <- coef(lasso_models[[name]], s = lasso_models[[name]]$lambda)

  
  # Convert the dgCMatrix to a regular matrix and then to a data frame
  coef_df <- as.data.frame(as.matrix(coef_matrix))
  
  # Only select the lambda values that correspond to the coefficients
  lambda_values <- lasso_models[[name]]$lambda[1:nrow(coef_df)]
  coef_df$lambda <- lambda_values
  
  coef_df <- coef_df %>% pivot_longer(-lambda, names_to = "Variable", values_to = "Coefficient")
  
  lasso_path_plot <- ggplot(coef_df, aes(x = lambda, y = Coefficient, color = Coefficient, group = Variable)) +
    geom_line() +
    scale_x_log10() +
    scale_color_viridis() +
    labs(title = paste("Coefficient Path (Lasso) for", name), x = "Log(Lambda)", y = "Coefficient Value") +
    theme_minimal() +
    ylim(-100, 100)  # Set y-axis limits
  
  ggsave(filename = paste0(plot_dir, "/", name, "_lasso_coefficient_path.png"), plot = lasso_path_plot, width = plot_width, height = plot_height, dpi = 300)
  
  # Lasso CV Error
  cv_error <- lasso_models[[name]]$cvm
  
  cv_df <- data.frame(lambda = lasso_models[[name]]$lambda, cv_error = cv_error)
  
  lasso_cv_plot <- ggplot(cv_df, aes(x = lambda, y = cv_error, color = cv_error)) +
    geom_line() +
    scale_x_log10() +
    scale_color_viridis() +
    labs(title = paste("CV Error (Lasso) for", name), x = "Log(Lambda)", y = "CV Error") +
    theme_minimal()
  
  ggsave(filename = paste0(plot_dir, "/", name, "_lasso_cv_error.png"), plot = lasso_cv_plot, width = plot_width, height = plot_height, dpi = 300)
  # Clean up temporary variables
  rm(fitted_and_residuals, plot, coef_df, lasso_path_plot, cv_error, cv_df, lasso_cv_plot, residuals_lasso, residuals_df, lasso_plot)
  }

# Visualization for Lasso Models
for (name in names(lasso_models)) {
  
  # Extract the coefficients for the current model
  coef_matrix <- coef(lasso_models[[name]], s = lasso_models[[name]]$lambda.min)
  
  # Convert the dgCMatrix to a regular matrix and then to a data frame
  coef_df <- as.data.frame(as.matrix(coef_matrix))
  coef_df$Variable <- rownames(coef_df) # Add a column for the variable names
  
  # Rename the coefficient column to match the linear regression plot
  colnames(coef_df)[1] <- "Estimate"
  
  # Filter for variables subsettable by _30, exclude prop_ variables, and exclude the intercept
  coef_df <- coef_df[grep("_30$", coef_df$Variable) & !grepl("^prop_", coef_df$Variable) & coef_df$Variable != "(Intercept)", ]
  
  # Lasso Variable Importance
  plot <- ggplot(coef_df, aes(x = reorder(Variable, Estimate), y = Estimate)) +
    geom_col(aes(fill = Estimate)) +
    scale_fill_viridis() +
    labs(title = paste("Variable Importance (Lasso) for", name), x = "Variables", y = "Coefficient Value") +
    coord_flip() +
    theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20))) # Center the title and add margin below
  
  ggsave(filename = paste0(plot_dir, "/", name, "_lasso_variable_importance.png"), plot = plot, width = vi_plot_width, height = vi_plot_height, dpi = 300)
}

# Model - Evaluation ----------------------------------

# Define directory for evaluation plots and tables
eval_dir <- "evaluations"
if (!dir.exists(eval_dir)) dir.create(eval_dir)

# Evaluation for Linear Models
for (name in names(linear_models)) {
  
  # Calculate R-squared
  r_squared <- summary(linear_models[[name]])$r.squared
  
  # Calculate Adjusted R-squared
  adj_r_squared <- summary(linear_models[[name]])$adj.r.squared
  
  # Root Mean Squared Error (RMSE)
  rmse <- sqrt(mean(residuals(linear_models[[name]])^2))
  
  # Save evaluation metrics to a CSV
  eval_df <- data.frame(Model = name, R_Squared = r_squared, Adjusted_R_Squared = adj_r_squared, RMSE = rmse)
  write.csv(eval_df, file = paste0(eval_dir, "/", name, "_linear_evaluation.csv"), row.names = FALSE)
}
  
# Assuming outcomes is your list of outcome variables
outcomes <- c("hle_years_male", "hle_years_female", "le_years_male", "le_years_female")

# Evaluation for Lasso Models
for (outcome in outcomes) {
  
  # Construct the model name
  model_name <- paste(outcome, "lasso", sep = "_")
  
  # Check if the model exists in lasso_models
  if (!model_name %in% names(lasso_models)) {
    next
  }
  
  # Predicted values using the Lasso model
  predicted_values <- predict(lasso_models[[model_name]], newx = model.matrix(model_formula, data = complete_data)[, -1], s = lasso_models[[model_name]]$lambda.min)
  
  # Actual values
  actual_values <- complete_data[[outcome]]
  
  # Calculate RMSE for Lasso
  rmse_lasso <- sqrt(mean((predicted_values - actual_values)^2))
  
  # Save evaluation metrics to a CSV
  eval_df_lasso <- data.frame(Model = model_name, RMSE = rmse_lasso)
  write.csv(eval_df_lasso, file = paste0(eval_dir, "/", model_name, "_evaluation.csv"), row.names = FALSE)
  
  # Residuals Plot for Lasso
  residuals_lasso <- actual_values - predicted_values
  residuals_df <- data.frame(Fitted = predicted_values, Residuals = residuals_lasso)
  
  # Explicitly set column names
  colnames(residuals_df) <- c("Fitted", "Residuals")
  
  lasso_plot <- ggplot(residuals_df, aes(x = Fitted, y = Residuals)) + 
    geom_point(aes(color = Residuals), alpha = 0.5) + 
    scale_color_viridis() + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    # Use model_name for the plot title and filename
    labs(title = paste("Residuals (Lasso) for", model_name), x = "Fitted Values", y = "Residuals")
  ggsave(filename = paste0(eval_dir, "/", model_name, "_residuals.png"), plot = lasso_plot, width = plot_width, height = plot_height)
  # Clean up temporary variables
  rm(predicted_values, actual_values, residuals_lasso, residuals_df, lasso_plot)
}



library(git2r)

# Initialize a new Git repository in your working directory
repo <- init()

# Add all files in your working directory to the Git repository
add(repo, "*")

# Commit the added files with a message
commit(repo, message = "Initial commit")

# Link your local repository to the GitHub repository 
# (replace 'YourUsername' and 'YourRepositoryName' with your actual GitHub username and repository name)
remote_add(repo, name = "origin", url = "C:/Users/blizc/OneDrive/Documents/GitHub/Transport-and-Health-Promoting-Resources-UK")

# Push the committed changes in your local repository to GitHub
push(repo, refspec = "refs/heads/master", credentials = cred_user_pass("becole", "BEce3709402!"))

