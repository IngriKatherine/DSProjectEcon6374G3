##############################################
# Author: Mohammed Al Alshaykh
# Course: ECON 6374 - Data Science Project
# Group: 3 (TX, LA)
# File: Mohammed_q7_q8_q9.R
# Description: Analysis for Questions 7, 8, and 9
##############################################

# Clean Environment
rm(list = ls())

# Load Packages
library(arrow)
library(dplyr)
library(tidyverse)

# Load dataset
lar <- read_parquet("proc_data/g3lar.parquet")

# Quick view of the dataset
glimpse(lar)
summary(lar)


##############################################
# Question 7: Open-end line of credit by ethnicity
##############################################

# 1) Filter the data with the 4 conditions (using exact labels)
q7_data <- lar %>%
  filter(
    loan_type == "Conventional (not insured or guaranteed by FHA VA RHS or FSA)",
    occupancy_type == "Principal residence",
    loan_purpose == "Home purchase",
    reverse_mortgage == "Not a reverse mortgage"
  )

# Check how many observations left after filtering
nrow(q7_data)

# Optional: see how many in each ethnicity group
table(q7_data$derived_ethnicity, useNA = "ifany")

# 2) Compute rate of open-end line of credit by ethnicity
q7_summary <- q7_data %>%
  group_by(derived_ethnicity) %>%
  summarize(
    n_obs   = n(),
    n_open  = sum(open_end_line_of_credit == "Open-end line of credit", na.rm = TRUE),
    rate_open = n_open / n_obs
  )

q7_summary


###############################################
# Question 8: County-level summary dataset
###############################################

# Create helper indicators for Q8
lar_q8 <- lar %>%
  mutate(
    
    # (b) Loan origination indicator
    is_origination = case_when(
      action_taken == "Loan originated" ~ 1,
      TRUE ~ 0
    ),
    
    # (c) Debt-to-income: extract lowest value in the range
    dti_str = as.character(debt_to_income_ratio),
    dti_clean = gsub("[^0-9]", "", dti_str),
    dti_low = suppressWarnings(as.numeric(substr(dti_clean, 1, 2))),
    dti_lt_45 = ifelse(!is.na(dti_low) & dti_low < 45, 1, 0),
    
    # (d) Freddie Mac indicator – Purchaser Type
    is_freddie = case_when(
      purchaser_type == "Freddie Mac" ~ 1,
      TRUE ~ 0
    ),
    
    # (e) Business or commercial purpose indicator
    is_business = case_when(
      business_or_commercial_purpose == 1 ~ 1,
      business_or_commercial_purpose == "Business or commercial purpose" ~ 1,
      TRUE ~ 0
    )
  )

# Create summary dataset by year, state, county
q8_summary <- lar_q8 %>%
  group_by(activity_year, state_code, county_code) %>%
  summarize(
    avg_interest_rate = mean(interest_rate, na.rm = TRUE),
    pct_origination = mean(is_origination, na.rm = TRUE),
    pct_dti_under_45 = mean(dti_lt_45, na.rm = TRUE),
    pct_freddie = mean(is_freddie, na.rm = TRUE),
    pct_business = mean(is_business, na.rm = TRUE),
    .groups = "drop"
  )

q8_summary

############################################################
# NOTE ABOUT QUESTION 9:
# The original question asks to compare loan approvals
# between years 2020 and 2022. 
# However, the dataset provided for this project does NOT 
# include observations for 2020 or 2022. The only available
# years in the processed dataset are 2021 and 2023.
#
# Therefore, to correctly complete the task while following 
# the same logic required by the question (long → wide reshape,
# compute percentage difference, and rank the top counties),
# the analysis below uses:
#   - 2021 as a substitute for 2020
#   - 2023 as a substitute for 2022
#
# This preserves the method and satisfies the question's 
# requirements using the available data.
############################################################


##############################################
# Question 9: Loan approvals by year and county
# (using 2021 and 2023 since these are the years in our dataset)
##############################################

# 1) Count loan approvals (Loan originated) by year and county
q9_counts <- lar %>%
  filter(
    action_taken == "Loan originated",
    activity_year %in% c(2021, 2023)
  ) %>%
  group_by(activity_year, state_code, county_code) %>%
  summarize(
    n_approvals = n(),
    .groups = "drop"
  )

# 2) Reshape from long to wide: one row per county, separate columns for 2021 and 2023
q9_wide <- q9_counts %>%
  tidyr::pivot_wider(
    names_from  = activity_year,
    values_from = n_approvals,
    names_prefix = "count_"
  )

# 3) Percentage difference between 2023 and 2021
q9_wide <- q9_wide %>%
  mutate(
    pct_diff = dplyr::if_else(
      !is.na(count_2021) & count_2021 > 0,
      (count_2023 - count_2021) / count_2021,
      NA_real_
    )
  )

# 4) Top 10 counties by absolute percentage change (formatted as percent)
q9_top10 <- q9_wide %>%
  filter(!is.na(pct_diff)) %>%
  mutate(
    pct_diff_abs   = abs(pct_diff),
    pct_diff_label = sprintf("%.2f%%", pct_diff * 100)
  ) %>%
  arrange(desc(pct_diff_abs)) %>%
  slice_head(n = 10) %>%
  select(
    state_code,
    county_code,
    count_2021,
    count_2023,
    pct_diff,
    pct_diff_label
  )

q9_top10

