###### Preamble
## Description:
# This code opens the raw dataset downloaded from Snapshot National Loan Level Dataset
# 2021 and 2023
# It filters the files to the states TX, LA
# Exports the dataset as a parquet file to save space

#Clean space
rm(list = ls())

#Libraries
library(dplyr)
library(tidyverse)
library(arrow)
library(openxlsx)

#Directory
getwd()
setwd("..")

###### Read 2021
lar2021 = read.csv("raw_data/2021_public_lar.csv", header = TRUE)
lar2021 = lar2021[!is.na(lar2021$state_code), ]
lar2021$g3 = ifelse(lar2021$state_code == "TX" | lar2021$state_code == "LA", 1, 0)

summary_stats <- lar2021 %>%
  group_by(g3) %>%
  summarise(
    median_loan_amount = median(loan_amount, na.rm = TRUE),
    average_interest_rate = mean(interest_rate, na.rm = TRUE)
  ) %>%
  mutate(
    Region = ifelse(g3 == 1, "TX and LA", "Other States")
  ) %>%
  select(Region, median_loan_amount, average_interest_rate)
print(summary_stats)

#Filter to only the states we need
TXLA2021 = lar2021 %>%filter(g3 == 1)

#Remove large DB
rm(lar2021)
#Free unused memory
gc()
# Dimension
print(dim(TXLA2021))
# Keep only the variables we need
TXLA2021=TXLA2021[c("state_code", "loan_amount", "interest_rate", "county_code", "activity_year", "lei", "loan_type", "derived_ethnicity", "action_taken", "applicant_age", "INDFMPIR")]


###### Read 2023
lar2023 <- read.csv("raw_data/2023_public_lar_csv.csv", header = TRUE)
lar2023 <- lar2023[!is.na(lar2023$state_code), ]
TXLA2023 <- lar2023[lar2023$state_code == "TX" | lar2023$state_code == "LA", ]
#Remove large DB
rm(lar2023)
# Dimension
print(dim(TXLA2023))
#Free unused memory
gc()

#### APPEND 2021 & 2023
TXLA <- rbind(TXLA2021, TXLA2023)
#Clean up
rm(lar2021)
#Free unused memory
gc()

###### Export Final DB
write_parquet(TXLA, "proc_data/TXLA_2021_2023.parquet")
