#Clean space
rm(list = ls())

#Libraries
library(dplyr)
library(tidyverse)
library(arrow)
library(ggplot2)
library(ggrepel)
library(cowplot)

#Directory
getwd()
setwd("..")

######
#Open DB
g3lar <- read_parquet("~/GitHub/DSProjectEcon6374G3/proc_data/g3lar.parquet")

fred_data <- read_parquet("~/GitHub/DSProjectEcon6374G3/proc_data/FREDLARTXLA.parquet")
names(fred_data)
head(fred_data)


##############
# Q20 – Creating logged variables


#Select numeric variables to log (exclude identifiers)
vars_to_log <- fred_data %>%
  dplyr::select(
    -county_code,
    -state,
    -year,
    -commute_time_bins
  ) %>%
  dplyr::select(where(is.numeric)) %>%
  names()

#Create logged versions using log1p (log(1 + x))
fred_data_logged <- fred_data %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(vars_to_log),
      ~ log1p(.x),
      .names = "log_{.col}"
    )
  )

#Quick checks
nrow(fred_data_logged)
names(fred_data_logged)

#Loan amount per capita and total loan amount are derived from average loan amount,  
#so even though they have a higher correlation, we use number of loans instead for 
#a more meaningful plot

#Plots
ggplot(fred_data_logged, aes(x = log_loan_amount_percapita, y = log_number_loans, color = as.factor(year))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Loan Amount and Number of Loans",
    x = "Log of Number of Loans ",
    y = "Log of Average Loan Value",
    color = "Year"
  ) +
  scale_x_continuous(
    breaks = seq(0, 100000000, 5000000),
    labels = scales::comma) +
  scale_y_continuous(
    breaks = seq(0, 200000, 10000),
    labels = scales::comma) +
  theme_minimal(base_size = 14)

fred_data$loan_amount_percapita

#20A Yes it shows a linear relationship. 


########
#21

#calculate outliers
outliers <- fred_data_binned %>%
  group_by(home_bin, year) %>% 
  mutate(
    Q1 = quantile(EQFXSUBPRIME, 0.25, na.rm = TRUE),
    Q3 = quantile(EQFXSUBPRIME, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR
  ) %>%
  filter(EQFXSUBPRIME < lower | EQFXSUBPRIME > upper) %>%
  ungroup


# Filter data for 2021
fred_2021 <- fred_data_binned %>% 
  filter(year == 2021)

fred_2023 <- fred_data_binned %>% 
  filter(year == 2023)

outliers_2021 <- outliers %>%
  filter(year == 2021)

outliers_2023 <- outliers %>%
  filter(year == 2023)

#Box Plots, outliers labeled by county code

#2021 Plot
ggplot(fred_2021, aes(x = home_bin, y = EQFXSUBPRIME)) +
  geom_boxplot(outlier.color = "red", outlier.alpha = 0.7, fill = "blue") +
  geom_point(
    data = outliers_2021,
    aes(x = home_bin, y = EQFXSUBPRIME),
    color = "red",
    alpha = 0.7,
    size = 2
  ) +
  geom_text_repel(
    data = outliers_2021,
    aes(label = county_code),
    color = "black",
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.4,
    point.padding = 0.3
  ) +
  labs(
    title = "Distribution of Subprime Mortgages by Home Ownership (2021)",
    x = "Home Ownership Bin",
    y = "Subprime Mortgage Rate",
  ) +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(fred_2023, aes(x = home_bin, y = EQFXSUBPRIME)) +
  geom_boxplot(outlier.color = "red", outlier.alpha = 0.7, fill = "green") +
  geom_point(
    data = outliers_2023,
    aes(x = home_bin, y = EQFXSUBPRIME),
    color = "red",
    alpha = 0.7,
    size = 2
  ) +
  geom_text_repel(
    data = outliers_2023,
    aes(label = county_code),
    color = "black",
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.4,
    point.padding = 0.3
  ) +
  labs(
    title = "Distribution of Subprime Mortgages by Home Ownership (2023)",
    x = "Home Ownership Bin",
    y = "Subprime Mortgage Rate",
    fill = "Home Ownership Bin"
  ) +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Violin Plots, outliers labeled by county code

#2021 Plot
ggplot(fred_2021, aes(x = home_bin, y = EQFXSUBPRIME)) +
  geom_violin(trim = FALSE, fill = "skyblue", alpha = 0.7) +
  geom_point(
    data = outliers_2021,
    aes(x = home_bin, y = EQFXSUBPRIME),
    color = "red",
    alpha = 0.7,
    size = 2
  ) +
  geom_text_repel(
    data = outliers_2021,
    aes(label = county_code),
    color = "black",
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.4,
    point.padding = 0.3
  ) +
  labs(
    title = "Distribution of Subprime Mortgages by Home Ownership (2021)",
    x = "Home Ownership Bin",
    y = "Subprime Mortgage Rate"
  ) +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#2023 Plot
ggplot(fred_2023, aes(x = home_bin, y = EQFXSUBPRIME)) +
  geom_violin(trim = FALSE, fill = "orange", alpha = 0.7) +
  geom_point(
    data = outliers_2023,
    aes(x = home_bin, y = EQFXSUBPRIME),
    color = "red",
    alpha = 0.7,
    size = 2
  ) +
  geom_text_repel(
    data = outliers_2023,
    aes(label = county_code),
    color = "black",
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.4,
    point.padding = 0.3
  ) +
  labs(
    title = "Distribution of Subprime Mortgages by Home Ownership (2023)",
    x = "Home Ownership Bin",
    y = "Subprime Mortgage Rate"
  ) +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







