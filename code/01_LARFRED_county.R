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
library(stringr)
library(labelled)
library(purrr)

#Directory
getwd()
setwd("..")

#################################################
###### Create Additional variables, combine with LAR
# And get final dataset for the last few points

FREDTXLA <- read_parquet("proc_data/FRED_LATX.parquet")

# A new variable “Commute Time Bins” with values of “Low” if below 20 minutes, 
#“Middle” if the value is between 20-40, and “High” above 40.
FREDTXLA <- FREDTXLA %>%
  rename(commute_time_mean = B080ACS) %>%
  mutate(
    commute_time_bins = cut(
      commute_time_mean,
      breaks = c(-Inf, 20, 40, Inf),
      labels = c("Low", "Middle", "High"),
      right = TRUE
    )
  )

# number of loans per capita (total divided by population).
### Bringing the loan dataset
lar = read_parquet("proc_data/larg3_county.parquet")

FREDLARTXLA <- list(FREDTXLA, lar) %>%
  reduce(full_join, by = c("year", "county_code"))

FREDLARTXLA$number_loans_percapita=FREDLARTXLA$number_loans/FREDLARTXLA$POPULATION
FREDLARTXLA$loan_amount_percapita=FREDLARTXLA$total_loan_amount/FREDLARTXLA$POPULATION

#### number of high school graduates per capita (total divided by population).
#this does not make sense

var_label(FREDLARTXLA) <- list(
  number_loans_percapita = "Number of originated loans Per Capita (action_taken == 'Loan originated')",
  loan_amount_percapita = "Total dollar volume of loans Per Capital"
)

write_parquet(FREDLARTXLA, "proc_data/FREDLARTXLA.parquet")

#End of R-script

