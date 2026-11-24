# ====================================
# Load Dataset
# ====================================

# Load required packages
library(dplyr)
library(lubridate)

# Load data from GitHub
df <- read.csv("https://raw.githubusercontent.com/wtjasmine/healthcare-data-cleaning/main/uncleaned_vaccine_data_demo.csv")

# View the dataset
df


# ====================================
# 1. Standardizing Patient Names
# ====================================

# Check patient names before cleaning
unique(df$Patient_Name)

# Convert names to lowercase
df$Patient_Name <- tolower(df$Patient_Name)

# Convert names to title case
df$Patient_Name <- tools::toTitleCase(df$Patient_Name)

# Check patient names after cleaning
unique(df$Patient_Name)


# ====================================
# 2. Standardizing Vaccine Types
# ====================================

# Check vaccine types before cleaning
unique(df$Vaccine_Type)

# Convert vaccine types to lowercase
df$Vaccine_Type <- tolower(df$Vaccine_Type)

# Convert vaccine types to title case
df$Vaccine_Type <- tools::toTitleCase(df$Vaccine_Type)

# Check vaccine types after cleaning
unique(df$Vaccine_Type)


# ====================================
# 3. Standardizing Date Formats
# ====================================

# Check dates before cleaning
df$Dose_Date

# Identify Excel numeric dates
excel_dates <- grepl("^[0-9]+$", df$Dose_Date)

# Convert Excel numeric dates into standard dates
df$Dose_Date[excel_dates] <- as.character(
  as.Date(as.numeric(df$Dose_Date[excel_dates]),
          origin = "1899-12-30")
)

# Convert different date formats into YYYY-MM-DD
df$Dose_Date <- as.Date(
  parse_date_time(df$Dose_Date,
                  orders = c("d/m/Y", "m/d/Y", "d-b-Y",
                             "m-d-Y", "d.m.Y", "Y.m.d",
                             "Ymd", "d/m/y", "Y/m/d H:M:S",
                             "Y-m-d"))
)

# Check dates after cleaning
df$Dose_Date


# ====================================
# 4. Standardizing Temperature Units
# ====================================

# Check temperature values before cleaning
df$Temperature

# Convert all letters to lowercase
df$Temperature <- tolower(df$Temperature)

# Remove spaces
df$Temperature <- gsub(" ", "", df$Temperature)

# Identify temperatures recorded in Fahrenheit
fahrenheit <- grepl("f", df$Temperature)

# Remove units and retain only numeric values
df$Temp_C <- as.numeric(gsub("[^0-9.\\-]", "", df$Temperature))

# Convert Fahrenheit to Celsius
df$Temp_C[fahrenheit] <- (df$Temp_C[fahrenheit] - 32) * 5/9

# Compare original and converted temperatures
df[, c("Temperature", "Temp_C")]


# ====================================
# 5. Handling Outliers and Missing Values
# ====================================

# Check age values before cleaning
df$Age

# Identify ages greater than 110
df$Age[df$Age > 110]

# Replace ages greater than 110 with NA
df$Age[df$Age > 110] <- NA

# Count missing age values
sum(is.na(df$Age))

# Calculate the mean age, excluding missing values
mean_age <- round(mean(df$Age, na.rm = TRUE))

# Replace missing ages with the mean age
df$Age[is.na(df$Age)] <- mean_age

# Check age values after cleaning
df$Age


# ====================================
# 6. Handling Inconsistent COVID Test Results
# ====================================

# Check unique values before cleaning
unique(df$Covid_Test)

# Convert all labels to lowercase
df$Covid_Test <- tolower(df$Covid_Test)

# Standardize positive results
df$Covid_Test[df$Covid_Test %in% c("+", "pos", "positive")] <- "positive"

# Standardize negative results
df$Covid_Test[df$Covid_Test %in% c("-", "neg", "negative")] <- "negative"

# Check unique values after cleaning
unique(df$Covid_Test)

# Frequency table
table(df$Covid_Test)


# ====================================
# 7. Handling Duplicate Records
# ====================================

# Identify duplicates based on patient name, vaccine type and dose date
duplicate_rows <- duplicated(df[, c("Patient_Name",
                                    "Vaccine_Type",
                                    "Dose_Date")])

# Count duplicate records
sum(duplicate_rows)

# View duplicate records
df[duplicate_rows, ]

# Remove duplicate records
df_clean <- df %>%
  distinct(Patient_Name, Vaccine_Type, Dose_Date, .keep_all = TRUE)

# Compare number of rows before and after cleaning
nrow(df)
nrow(df_clean)


# ====================================
# Final Cleaned Dataset
# ====================================

# View cleaned dataset
df_clean

# Check remaining missing values
colSums(is.na(df_clean))
