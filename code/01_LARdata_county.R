###### Preamble
## Description:
# This code opens the filtered FIECS dataset TX, LA
#	It create a new dataset that is summary statistics by year, state, and county. 
# This dataset includes
#Average interest rate per county. 
#Percentage of “actions taken” for loan origination (versus all other values). 
#Percentage of debt_to_income_ratio that are less than 45%
#This one will require recoding because of ranges.  Suggest using gsub to remove punctuation and then take the first two characters and make them a new variable. But we want the lowest percent for each one, you cannot just set the ranges to missing. 
#Percentage of loans that are for Freddie Mac (Type of Purchaser)
#Percentage of loans that are “business or commercial purpose”  

#Clean space
rm(list = ls())

#Libraries
library(dplyr)
library(tidyverse)
library(arrow)
library(labelled)

#Directory
getwd()
setwd("..")

###############################################
# County-level dataset
###############################################
lar = read_parquet("proc_data/g3lar.parquet")

# Create indicators
lar = lar %>%
  mutate(
    # 1. Indicator: loan originated
    is_origination = if_else(action_taken == "Loan originated", 1, 0),
    
    # 2. Indicator: refinancing
    is_refinance = if_else(
      loan_purpose %in% c("Refinancing", "Cash-out refinancing"),
      1, 0
    ),
    
    # 3. Indicator: high-cost / subprime mortgage
    is_subprime = if_else(
      hoepa_status == "High-cost mortgage",
      1, 0
    )
  )

# Summaries by year and county
larg3_county <- lar %>%
  group_by(activity_year, county_code) %>%
  summarize(
    number_loans = sum(is_origination, na.rm = TRUE),
    total_loan_amount = sum(loan_amount, na.rm = TRUE),
    average_loan_amount = mean(loan_amount, na.rm = TRUE),
    
    percentage_loan_refinancing = mean(is_refinance, na.rm = TRUE) * 100,
    percentage_subprime_mortgage = mean(is_subprime, na.rm = TRUE) * 100,
    
    .groups = "drop"
  ) %>%
  # Add descriptive labels
  rename(
    year = activity_year
  )

var_label(larg3_county) <- list(
  year = "Activity year of HMDA record",
  county_code = "County FIPS 5 dig code",
  number_loans = "Number of originated loans (action_taken == 'Loan originated')",
  total_loan_amount = "Total dollar volume of loans (sum of loan_amount)",
  average_loan_amount = "Mean loan amount among all observations",
  percentage_loan_refinancing = "Percent of loans with loan_purpose = Refinancing or Cash-out refinancing",
  percentage_subprime_mortgage = "Percent of loans with hoepa_status = High-cost mortgage"
)

write_parquet(larg3_county, "proc_data/larg3_county.parquet")

#End of R-script