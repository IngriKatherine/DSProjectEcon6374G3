###### Preamble
## Description:
# This code opens the filtered FIECS dataset TX, LA

#Clean space
rm(list = ls())

#Libraries
library(dplyr)
library(tidyverse)
library(arrow)

#Directory
getwd()
setwd("..")

######
#Open DB
g3lar <- read_parquet("~/GitHub/DSProjectEcon6374G3/proc_data/g3lar.parquet")

str(g3lar)
summary(g3lar)
table(g3lar$state_code, useNA = "ifany")

g3lar$loan_type_label <- recode(g3lar$loan_type,
                             "1" = "Conventional",
                             "2" = "FHA",
                             "3" = "VA",
                             "4" = "USDA")

table_a <- g3lar %>%
  group_by(loan_type_label) %>%
  summarize(
    min_value  = min(loan_amount, na.rm = TRUE),
    max_value  = max(loan_amount, na.rm = TRUE),
    mean_value = mean(loan_amount, na.rm = TRUE)
  )

table_a

avg_plot_data <- g3lar %>%
  group_by(state_code, activity_year, loan_type_label) %>%
  summarize(avg_value = mean(loan_amount, na.rm = TRUE))

library(ggplot2)

ggplot(avg_plot_data, aes(x = state_code, y = avg_value, fill = loan_type_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(avg_value, 0)),
            position = position_dodge(width = 0.9),
            vjust = -0.25,
            size = 3) +
  facet_wrap(~ activity_year) +
  labs(
    title = "Average Loan Value by Loan Type, State, and Year",
    x = "State",
    y = "Average Loan Value($)",
    fill = "Loan Type"
  ) +
  scale_y_continuous(
    breaks = seq(0, 400000, 50000),
    labels = scales::comma) +
  theme_bw()

