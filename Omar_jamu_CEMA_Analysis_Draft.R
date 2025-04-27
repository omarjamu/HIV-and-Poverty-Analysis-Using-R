# Install required packages
packages <- c("tidyverse", "ggplot2", "sf", "leaflet", "lme4", "ggeffects", 
              "RColorBrewer", "knitr", "rmarkdown", "viridis")

install.packages(setdiff(packages, rownames(installed.packages())))

# Load libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(leaflet)
library(lme4)
library(ggeffects)
library(RColorBrewer)
library(knitr)
library(rmarkdown)
library(viridis)
library(readxl)  # For reading Excel files

# PART 1: HIV Data Analysis

# Load data
hiv_data <- read_csv("C:/Users/Test/Downloads/HIV data 2000-2023.csv")

# View the column names of the HIV dataset
colnames(hiv_data)

hiv_data <- hiv_data %>%
  mutate(
    Value_clean = str_remove_all(Value, "\\[.*?\\]"),   # Remove square bracket parts
    Value_clean = str_replace_all(Value_clean, "[^0-9]", ""),  # Keep only digits
    Value_clean = as.numeric(Value_clean)               # Now convert
  ) %>%
  filter(!is.na(Value_clean))


# 1a. HIV Trends: Countries Contributing to 75% of Global Burden
# ----------------------------------------
# Filter data for 2023
hiv_2023 <- hiv_data %>%
  filter(Period == 2023) %>%
  arrange(desc(Value_clean)) %>%
  mutate(Cumulative = cumsum(Value_clean) / sum(Value_clean)) %>%
  filter(Cumulative <= 0.75)

# List of top 75% countries
top_75_countries <- hiv_2023$Location
top_75_countries 

# Filter full data for these countries
hiv_top <- hiv_data %>%
  filter(Location %in% top_75_countries)

# Plot trend for these countries
ggplot(hiv_top, aes(x = Period, y = Value_clean, color = Location)) +
  geom_line() +
  labs(title = "HIV Trends in Top 75% Global Burden Countries (2000-2023)",
       x = "Year",
       y = "Number of People Living with HIV") +
  theme_minimal()


# 1b. HIV Trends: Top 75% Countries within each WHO Region
# ----------------------------------------
# Find top 75% countries within each region
top_75_by_region <- hiv_data %>%
  filter(Period == 2023) %>%  # Use 'Period' instead of 'Year'
  group_by(ParentLocationCode) %>% 
  arrange(ParentLocationCode, desc(Value_clean)) %>%
  mutate(Cumulative = cumsum(Value_clean) / sum(Value_clean)) %>%
  filter(Cumulative <= 0.75)

# Filter the top 75% countries data to plot the trends
hiv_region_top <- hiv_data %>%
  filter(Location %in% top_75_by_region$Location)  # Keep only the top 75% countries

# Plot the HIV trends for each WHO region with slanted x-axis labels
ggplot(hiv_region_top, aes(x = Period, y = Value_clean, color = Location)) +
  geom_line() +
  facet_wrap(~ ParentLocationCode, scales = "free_y") +  # Use WHO region as facet
  labs(title = "HIV Trends by WHO Region (Top 75% Countries in 2023)",
       x = "Year",
       y = "Number of People Living with HIV") +
  theme_minimal() +
  theme(
    legend.position = "none",              # Remove the legend
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels by 45 degrees
  )

# 1c: Relationship Between HIV and Multidimensional Poverty

# Select the relevant variables from the HIV dataset
hiv_selected <- hiv_data %>%
  select(Location, Period, Value_clean)  # Adjust according to the columns available

# View the selected data
head(hiv_selected)

#Import and Read the Poverty Dataset
poverty_data <- read_excel("C:/Users/Test/Downloads/multidimensional_poverty.xlsx")

# Check the first few rows to ensure it loaded correctly
head(poverty_data)
colnames(poverty_data)

# Select the important variables from the poverty dataset
poverty_selected <- poverty_data %>%
  select(Economy, 
         `Country code`, 
         'Reporting year', 
         'Survey year', 
         `Monetary (%)`, 
         `Educational attainment (%)`, 
         `Educational enrollment (%)`, 
         `Electricity (%)`, 
         `Sanitation (%)`, 
         `Drinking water (%)`, 
         `Multidimensional poverty headcount ratio (%)`)

# View the selected data
head(poverty_selected)

# Perform the join on the 'Economy' from poverty data and 'Location' from hiv data
merged_data <- poverty_selected %>%
  left_join(hiv_selected, by = c("Economy" = "Location"))

# View the merged data
head(merged_data)

# Perform a correlation analysis between Multidimensional poverty headcount ratio and HIV data
correlation_result <- cor(merged_data$`Multidimensional poverty headcount ratio (%)`, 
                          merged_data$Value_clean, 
                          use = "complete.obs")  # Handle missing values

# Print the result
print(correlation_result)

# Perform linear regression to analyze the relationship between multidimensional poverty and HIV
regression_model <- lm(Value_clean ~ 
                       `Multidimensional poverty headcount ratio (%)` + 
                       `Monetary (%)` + 
                       `Educational attainment (%)` + 
                       `Educational enrollment (%)` + 
                       `Electricity (%)` + 
                       `Sanitation (%)` + 
                       `Drinking water (%)` + 
                       `Survey year`, 
                       data = merged_data)

# Summary of the regression model
summary(regression_model)


# PART 2: Mortality Analysis in East Africa

# Load mortality data
mortality_data <- read_csv("C:/Users/Test/Downloads/dataset_datascience.csv")

colnames(mortality_data)

head(mortality_data)

eac_countries <- c("Kenya", "Uganda", "Tanzania", "Rwanda", "Burundi", 
                   "South Sudan", "DR Congo", "Somalia")


# Filter the data for EAC countries and the relevant indicators
eac_mortality_filtered <- mortality_data %>%
  filter(`Geographic area` %in% eac_countries) %>%  # Filter for EAC countries
  filter(Indicator %in% c("Neonatal mortality rate", "Under-five mortality rate"))  # Filter for relevant indicators

# View the first few rows of the filtered data
head(eac_mortality_filtered)










# Step 2: Clean "Series Year" (sometimes you have messy values like "2022-2023")
eac_mortality_filtered <- eac_mortality_filtered %>%
  mutate(`Series Year Clean` = as.numeric(str_sub(`Series Year`, 1, 4)))

# Step 3: Get the latest available estimate per country and indicator
eac_mortality_latest <- eac_mortality_filtered %>%
  filter(!is.na(`Series Year Clean`)) %>%
  group_by(`Geographic area`, Indicator) %>%
  filter(`Series Year Clean` == max(`Series Year Clean`)) %>%
  ungroup()

# Step 4: Visualize the latest estimate for each indicator (Bar plots)

# Plot for Under-five mortality rate
under_five_latest <- eac_mortality_latest %>%
  filter(Indicator == "Under-five mortality rate")

ggplot(under_five_latest, aes(x = reorder(`Geographic area`, -`Observation Value`), 
                              y = `Observation Value`, fill = `Geographic area`)) +
  geom_bar(stat = "identity") +
  labs(title = "Latest Under-Five Mortality Rate in EAC Countries",
       x = "Country",
       y = "Deaths per 1000 live births") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for Neonatal mortality rate
neonatal_latest <- eac_mortality_latest %>%
  filter(Indicator == "Neonatal mortality rate")

ggplot(neonatal_latest, aes(x = reorder(`Geographic area`, -`Observation Value`), 
                             y = `Observation Value`, fill = `Geographic area`)) +
  geom_bar(stat = "identity") +
  labs(title = "Latest Neonatal Mortality Rate in EAC Countries",
       x = "Country",
       y = "Deaths per 1000 live births") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 5: Show the trend over time (Average trends)

# Calculate average mortality per year
eac_mortality_trends <- eac_mortality_filtered %>%
  filter(!is.na(`Series Year Clean`)) %>%
  group_by(`Series Year Clean`, Indicator) %>%
  summarise(Average_Mortality = mean(`Observation Value`, na.rm = TRUE)) %>%
  ungroup()

# Plot for Under-five mortality trends
eac_mortality_trends_under5 <- eac_mortality_trends %>%
  filter(Indicator == "Under-five mortality rate")

ggplot(eac_mortality_trends_under5, aes(x = `Series Year Clean`, y = Average_Mortality)) +
  geom_line(color = "blue") +
  geom_point(data = eac_mortality_filtered %>% filter(Indicator == "Under-five mortality rate"),
             aes(x = `Series Year Clean`, y = `Observation Value`, color = `Geographic area`),
             alpha = 0.6) +
  labs(title = "Under-Five Mortality Rate Trends (Average and Country Points)",
       x = "Year",
       y = "Deaths per 1000 live births") +
  theme_minimal()

# Plot for Neonatal mortality trends
eac_mortality_trends_neonatal <- eac_mortality_trends %>%
  filter(Indicator == "Neonatal mortality rate")

ggplot(eac_mortality_trends_neonatal, aes(x = `Series Year Clean`, y = Average_Mortality)) +
  geom_line(color = "red") +
  geom_point(data = eac_mortality_filtered %>% filter(Indicator == "Neonatal mortality rate"),
             aes(x = `Series Year Clean`, y = `Observation Value`, color = `Geographic area`),
             alpha = 0.6) +
  labs(title = "Neonatal Mortality Rate Trends (Average and Country Points)",
       x = "Year",
       y = "Deaths per 1000 live births") +
  theme_minimal()

# Step 6: Identify the countries with the highest mortality rates

# Highest under-five mortality
highest_under5 <- under_five_latest %>%
  arrange(desc(`Observation Value`)) %>%
  slice(1)

# Highest neonatal mortality
highest_neonatal <- neonatal_latest %>%
  arrange(desc(`Observation Value`)) %>%
  slice(1)

# Print
print(paste("Country with the highest Under-Five Mortality Rate:", highest_under5$`Geographic area`))
print(paste("Country with the highest Neonatal Mortality Rate:", highest_neonatal$`Geographic area`))

