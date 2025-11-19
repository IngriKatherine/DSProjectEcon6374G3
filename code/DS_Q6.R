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
#6a
g3lar$loan_type_label <- recode(g3lar$loan_type,
                             "1" = "Conventional",
                             "2" = "FHA",
                             "3" = "VA",
                             "4" = "USDA")

table_6a <- g3lar %>%
  group_by(loan_type_label) %>%
  summarize(
    min_value  = min(loan_amount, na.rm = TRUE),
    max_value  = max(loan_amount, na.rm = TRUE),
    mean_value = mean(loan_amount, na.rm = TRUE)
  )

table_6a

avg_plot_data <- g3lar %>%
  group_by(state_code, activity_year, loan_type_label) %>%
  summarize(avg_value = mean(loan_amount, na.rm = TRUE))

library(ggplot2)
#6b
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
#6c
g3lar$property_type <- recode(g3lar$construction_method,
                                 "1" = "Site-built",
                                 "2" = "Manufactured housing")
table_6c <- g3lar %>%
  group_by(activity_year, property_type) %>%
  summarize(avg_value = mean(loan_amount, na.rm = TRUE))

table_6c


#6d
table_6d <- table(g3lar$action_taken, g3lar$occupancy_type, useNA = "always")
table_6d

percent_not_applicable <- mean(g3lar$action_taken == "Not Applicable", na.rm = TRUE)
percent_not_applicable

ggplot(g3lar, aes(x = action_taken, fill = occupancy_type)) +
  geom_bar(position = "dodge") + 
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3.2
  ) +
  labs(
    title = "Action Taken by Occupancy Type",
    x = "Action Taken",
    y = "Count",
    fill = "Occupancy Type"
  ) +
  scale_y_continuous(
    breaks = seq(0, 5000000, 100000),
    labels = scales::comma) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Due to the large differnce in Values, it was difficult to see certain variables on the bar chart, so
# we added separate bar charts for each occupancy type
ggplot(g3lar %>% filter(occupancy_type == "Principal residence"), 
       aes(x = action_taken)) +
  geom_bar(fill = "red") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Action Taken — Principal Residence",
    x = "Action Taken",
    y = "Count"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(g3lar %>% filter(occupancy_type == "Second residence"), 
       aes(x = action_taken)) +
  geom_bar(fill = "green") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Action Taken — Second Residence",
    x = "Action Taken",
    y = "Count"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(g3lar %>% filter(occupancy_type == "Investment property"), 
       aes(x = action_taken)) +
  geom_bar(fill = "blue") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Action Taken — Investment Property",
    x = "Action Taken",
    y = "Count"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

