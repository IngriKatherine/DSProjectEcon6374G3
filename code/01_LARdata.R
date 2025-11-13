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

###### FUNCTION INITIAL CLEANING ALL STATES ######
process_lar_year <- function(year) {
  # 1. Read in data
  file_path <- sprintf("raw_data/%d_public_lar.csv", year)
  lar <- read.csv(file_path, header = TRUE)
  
  # 2. Drop missings in state
  lar <- lar[!is.na(lar$state_code), ]
  
  # 3. Keep only the variables we need
  varstokeep <- c(
    "activity_year", "state_code","county_code", 
    "construction_method", "derived_ethnicity", 
    "action_taken","purchaser_type", "loan_type","loan_purpose",
    "reverse_mortgage", "open_end_line_of_credit",
    "business_or_commercial_purpose", "loan_amount",
    "interest_rate", "debt_to_income_ratio", "occupancy_type"
  )
  lar <- lar[varstokeep]
  
  # 4. Create variable that identifies Group 3 states
  lar$g3 <- ifelse(lar$state_code == "TX" | lar$state_code == "LA", 1, 0)
  
  # 5. Convert Interest Rate and Loan Amount to Numeric
  # and Fix debt to income ratio variable
  lar$interest_rate <- suppressWarnings(as.numeric(lar$interest_rate))
  lar$loan_amount   <- suppressWarnings(as.numeric(lar$loan_amount))
  
  lar <- lar %>%
    mutate(
      # Step 1: numeric "minimum DTI" using gsub + first 2 digits
      dti_digits = gsub("[^0-9]", "", debt_to_income_ratio),                 
      dti_min    = suppressWarnings(as.numeric(substr(dti_digits, 1, 2))),
      
      # Step 2: assign everything (ranges + exact values) into interval buckets
      debt_to_income_ratio = case_when(
        debt_to_income_ratio %in% c("Exempt") ~ "Exempt",
        !is.na(dti_min) & dti_min < 20                ~ "<20%",
        !is.na(dti_min) & dti_min >= 20 & dti_min < 30 ~ "20%-<30%",
        !is.na(dti_min) & dti_min >= 30 & dti_min < 36 ~ "30%-<36%",
        !is.na(dti_min) & dti_min >= 36 & dti_min < 50 ~ "36%-<50%",
        !is.na(dti_min) & dti_min >= 50 & dti_min <= 60 ~ "50%-60%",
        !is.na(dti_min) & dti_min > 60                ~ ">60%",
        TRUE ~ NA_character_
      ),
      
      # Make it an ordered factor in the desired order
      debt_to_income_ratio = factor(
        debt_to_income_ratio,
        levels = c(
          "<20%",
          "20%-<30%",
          "30%-<36%",
          "36%-<50%",
          "50%-60%",
          ">60%",
          "Exempt"
        ),
        ordered = TRUE
      )
    ) %>%
    select(-dti_digits, -dti_min)
  
  # 6. Convert to factors and add levels to everything else
  lar <- lar %>%
    mutate(
      #States Group3
      g3 = factor(
        g3,
        levels = c(0,1),
        labels = c(
          "Other States",
          "Texas and Louisiana"
        )
      ),
      # Ethnicity of Applicant or Borrower
      derived_ethnicity = factor(derived_ethnicity),
      # Action Taken
      action_taken = factor(
        action_taken,
        levels = 1:8,
        labels = c(
          "Loan originated",
          "Application approved but not accepted",
          "Application denied",
          "Application withdrawn by applicant",
          "File closed for incompleteness",
          "Purchased loan",
          "Preapproval request denied",
          "Preapproval request approved but not accepted"
        )
      ),
      # Purchaser Type
      purchaser_type = factor(
        purchaser_type,
        levels = c(0, 1, 2, 3, 4, 5, 6, 71, 72, 8, 9),
        labels = c(
          "Not applicable",
          "Fannie Mae",
          "Ginnie Mae",
          "Freddie Mac",
          "Farmer Mac",
          "Private securitizer",
          "Commercial bank savings bank or savings association",
          "Credit union mortgage company or finance company",
          "Life insurance Company",
          "Affiliate institution",
          "Other type of purchaser"
        )
      ),
      # Loan Type
      loan_type = factor(
        loan_type,
        levels = c(1, 2, 3, 4),
        labels = c(
          "Conventional (not insured or guaranteed by FHA VA RHS or FSA)",
          "Federal Housing Administration insured (FHA)",
          "Veterans Affairs guaranteed (VA)",
          "USDA RHS or FSA guaranteed"
        )
      ),
      # Loan Purpose
      loan_purpose = factor(
        loan_purpose,
        levels = c(1, 2, 31, 32, 4, 5),
        labels = c(
          "Home purchase",
          "Home improvement",
          "Refinancing",
          "Cash-out refinancing",
          "Other purpose",
          "Not applicable"
        )
      ),
      # Reverse Mortgage
      reverse_mortgage = factor(
        reverse_mortgage,
        levels = c(1, 2),
        labels = c(
          "Reverse mortgage",
          "Not a reverse mortgage"
        )
      ),
      # Open-End Line of Credit
      open_end_line_of_credit = factor(
        open_end_line_of_credit,
        levels = c(1, 2),
        labels = c(
          "Open-end line of credit",
          "Not an open-end line of credit"
        )
      ),
      # Business or Commercial Purpose
      business_or_commercial_purpose = factor(
        business_or_commercial_purpose,
        levels = c(1, 2),
        labels = c(
          "Primarily for a business or commercial purpose",
          "Not primarily for a business or commercial purpose"
        )
      ),
      # Construction Method
      construction_method = factor(
        construction_method,
        levels = c(1, 2),
        labels = c(
          "Site-built",
          "Manufactured Home"
        )
      ),
      # Occupancy Type
      occupancy_type = factor(
        occupancy_type,
        levels = c(1, 2, 3),
        labels = c(
          "Principal residence",
          "Second residence",
          "Investment property"
        )
      ),
    )
  
  # 7. Add variable labels
  var_labels <- c(
    activity_year                   = "Year",
    state_code                      = "State 2dig code",
    county_code                     = "County 5dig code",
    derived_ethnicity               = "Ethnicity of Applicant or Borrower",
    action_taken                    = "Action Taken",
    purchaser_type                  = "Type of Purchaser",
    loan_type                       = "Loan Type",
    loan_purpose                    = "Loan Purpose",
    reverse_mortgage                = "Reverse Mortgage",
    open_end_line_of_credit         = "Open-End Line of Credit",
    business_or_commercial_purpose  = "Business or Commercial Purpose",
    loan_amount                     = "Loan Amount",
    interest_rate                   = "Interest Rate",
    debt_to_income_ratio            = "Debt-to-Income Ratio",
    construction_method             = "Construction Method",
    occupancy_type                  = "Occupancy Type",
    g3                              = "Dummy Group 3 States"
  )
  
  for (v in names(var_labels)) {
    if (v %in% names(lar)) {
      attr(lar[[v]], "label") <- var_labels[[v]]
    }
  }
  
  # 8. TABLE G3 vs OTHER
  summary_g3vsoth <- lar %>%
    group_by(g3) %>%
    summarise(
      median_loan_amount = median(loan_amount, na.rm = TRUE),
      average_interest_rate = mean(interest_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # 2. Format for display
    mutate(
      # Dollar sign + commas (no decimals for loan amount, adjust if you want cents)
      median_loan_amount = paste0(
        "$", format(round(median_loan_amount, 0), big.mark = ",", scientific = FALSE)
      ),
      # Interest rate to 2 decimal places
      average_interest_rate = sprintf("%.2f", average_interest_rate)
    )
  
  # 3. Create a copy with "variable labels" as column names
  summary_g3vsoth_export <- summary_g3vsoth
  names(summary_g3vsoth_export) <- c(
    "Group (G3 vs Other)",
    "Median Loan Amount ($)",
    "Average Interest Rate"
  )
  
  # 4. Export to Excel
  outname <- sprintf("output/comparison_g3vsother_%d.xlsx", year)
  write.xlsx(summary_g3vsoth_export, file = outname, overwrite = TRUE)
  # 9. FILTER TO ONLY THE STATES WE NEED
  g3lar <- lar %>% filter(g3 == "Texas and Louisiana")
  
  ##  Small additional cleaning
  # Pad the county code digit to have leading 0ros
  g3lar$county_code <- sprintf("%05d", g3lar$county_code)
  #Keep only the first 3 digits
  g3lar$county_code <- substr(g3lar$county_code, 1, 3)
  
  return(g3lar)
}

###### RUN BY YEAR ######

## ---- 2021 ----
g3lar2021 <- process_lar_year(2021)

## ---- 2023 ----
g3lar2023 <- process_lar_year(2023)

###### APPEND 2021 & 2023 ######
g3lar <- rbind(g3lar2021, g3lar2023)

###### SAVE FINAL DATASET ######
write_parquet(g3lar, "proc_data/g3lar.parquet")

#Free unused memory and R.Data
rm(list = ls())
gc()

#End of R-script