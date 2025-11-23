############################################################
# Question 19 – Correlations with loan amount
############################################################

# Clean environment
rm(list = ls())

# Load packages
library(arrow)
library(dplyr)
library(tidyr)
library(tidyverse)

# Load dataset
ds <- read_parquet("proc_data/FREDLARTXLA.parquet")

# Quick look at the data
glimpse(ds)
summary(ds)
names(ds)


############################################################
# Q19(a) – Subset to first year and create logged variables
############################################################

# 1) Keep only the first year in the dataset
first_year <- min(ds$year, na.rm = TRUE)

ds_1year <- ds %>%
  dplyr::filter(year == first_year)

# 2) Select numeric variables to log (exclude identifiers)
vars_to_log <- ds_1year %>%
  dplyr::select(
    -county_code,
    -state,
    -year,
    -commute_time_bins
  ) %>%
  dplyr::select(where(is.numeric)) %>%
  names()

# 3) Create logged versions using log1p (log(1 + x))
ds_1year <- ds_1year %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(vars_to_log),
      ~ log1p(.x),
      .names = "log_{.col}"
    )
  )

# 4) Quick checks
nrow(ds_1year)
names(ds_1year)


############################################################
# Q19(b) – Correlation matrix for added variables (excluding loan-related)
############################################################

# 1) Variables NOT derived from loan amount
non_loan_vars <- c(
  "EQFXSUBPRIME", "MEDAONMACOUNTY", "HOWNRATEACS", 
  "PPAAWY", "commute_time_mean", "FBITC", 
  "HC01ESTVC16", "SNAPBR", "POPULATION"
)

# 2) Include logged versions of these variables
log_vars <- paste0("log_", non_loan_vars)

# 3) Combine them
vars_for_corr <- c(non_loan_vars, log_vars)

# 4) Create correlation matrix using only first-year data
corr_matrix <- ds_1year %>%
  dplyr::select(all_of(vars_for_corr)) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)

# 5) Print correlation matrix
corr_matrix


############################################################
# Q19(b) – Identify pairs with |correlation| > 0.85
############################################################

high_corr_pairs <- corr_matrix %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var1") %>%
  tidyr::pivot_longer(
    cols = -var1,
    names_to = "var2",
    values_to = "corr"
  ) %>%
  dplyr::filter(var1 < var2) %>%        # avoid duplicates
  dplyr::filter(abs(corr) > 0.85)       # correlations above threshold

high_corr_pairs

############################################################
# Q19(c) – Correlation matrix: all logged vars vs loan amount
############################################################

# 1) Choose the loan amount variable (logged)
loan_var <- "log_loan_amount_percapita"

# 2) Get all logged variables
logged_vars <- grep("^log_", names(ds_1year), value = TRUE)

# Make sure loan_var is first and not duplicated
logged_vars <- setdiff(logged_vars, loan_var)
vars_for_corr_c <- c(loan_var, logged_vars)

# 3) Correlation matrix (logged vars vs logged loan amount)
corr_logged_vs_loan <- ds_1year %>%
  dplyr::select(all_of(vars_for_corr_c)) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)

# 4) View full correlation matrix
corr_logged_vs_loan

# (Optional) If you want only the correlations with log_loan_amount_percapita:
corr_with_loan_only <- corr_logged_vs_loan[, loan_var, drop = FALSE]
corr_with_loan_only


