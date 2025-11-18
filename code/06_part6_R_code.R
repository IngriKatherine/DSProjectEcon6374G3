###############################################################################
# PART 6 — Loan Value Statistical Summaries
# Dataset: g3lar.parquet (TX & LA HMDA records for 2021 & 2023)
# Author: Husam + Group 3
# Course: ECON 6374 – Data Science
###############################################################################

library(dplyr)
library(ggplot2)

###############################################################################
# 6(a) Summary table: Min, Max, and Mean Loan Amount by Loan Type
###############################################################################

table_6a <- data %>%
  group_by(loan_type) %>%
  summarise(
    min_loan  = min(loan_amount, na.rm = TRUE),
    max_loan  = max(loan_amount, na.rm = TRUE),
    mean_loan = mean(loan_amount, na.rm = TRUE)
  )

table_6a



###############################################################################
# 6(b) Clean & Professional Bar Chart (Conventional only, NA dropped)
###############################################################################

library(dplyr)
library(ggplot2)
library(scales)

# Map your full loan_type labels to short names
short_labels <- c(
  "Conventional (not insured or guaranteed by FHA VA RHS or FSA)" = "Conventional",
  "FHA insured" = "FHA",
  "VA guaranteed" = "VA",
  "USDA Rural Housing Service guaranteed" = "USDA",
  "Not Applicable" = "Other / NA"
)

plot_6b_data <- data %>%
  group_by(state_code, activity_year, loan_type) %>%
  summarise(
    avg_loan = mean(loan_amount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    loan_type_short = short_labels[as.character(loan_type)]
  ) %>%
  # Do NOT plot the Not Applicable category
  filter(loan_type_short != "Other / NA")

plot_6b <- ggplot(plot_6b_data,
                  aes(x = loan_type_short, y = avg_loan, fill = state_code)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = comma(round(avg_loan, 0))),
    position = position_dodge(width = 0.7),
    vjust = -0.3,
    size  = 3.8,
    color = "black",
    fontface = "bold"
  ) +
  facet_wrap(~ activity_year, ncol = 2) +
  scale_fill_manual(values = c("LA" = "#E76F51", "TX" = "#2A9D8F"),
                    name = "State") +
  scale_y_continuous(
    labels = comma_format(),
    expand = expansion(mult = c(0, 0.08))   # small headroom for labels
  ) +
  labs(
    title = "Average Loan Amount by Loan Type, State, and Year",
    subtitle = "States: Louisiana (LA) and Texas (TX)",
    x = "Loan Type",
    y = "Average Loan Amount ($)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.title.x  = element_text(size = 12, margin = margin(t = 8)),
    axis.title.y  = element_text(size = 12, margin = margin(r = 8)),
    strip.text    = element_text(face = "bold", size = 13),
    legend.position = "right",
    plot.margin = margin(10, 20, 10, 10)
  )

plot_6b

###############################################################################
# 6(c) Average Loan Amount for 2021 & 2023 by Construction Method
###############################################################################

library(dplyr)
library(scales)

# Create table summarizing average loan amount by construction method
table_6c <- data %>%
  filter(activity_year %in% c(2021, 2023)) %>%               # Only 2021 and 2023
  group_by(activity_year, construction_method) %>%           # Group by year + method
  summarise(
    avg_loan_amount = mean(loan_amount, na.rm = TRUE),       # Compute average
    count = n(),                                             # Optional: sample size
    .groups = "drop"
  ) %>%
  mutate(
    avg_loan_amount = round(avg_loan_amount, 2)              # Nice formatting
  )

table_6c

###############################################################################
# 6(d)(i) Two-way table of Action Taken × Occupancy Type (counts only)
###############################################################################

table_6d <- table(
  Action_Taken     = data$action_taken,
  Occupancy_Type   = data$occupancy_type
)

table_6d

###############################################################################
# 6(d)(ii) Percentage of observations with Occupancy Type = "Not Applicable"
###############################################################################

pct_not_app_occ <- mean(data$occupancy_type == "Not Applicable", na.rm = TRUE) * 100
pct_not_app_occ

# No records in the dataset are classified as “Not Applicable,” resulting in a 0% share for this category.

###############################################################################
# 6(d)(iii) Final Clean Plot — Labels Only for Large Bars
###############################################################################

library(dplyr)
library(ggplot2)
library(scales)

plot_6d_data <- data %>%
  count(action_taken, occupancy_type, name = "count")

ggplot(plot_6d_data,
       aes(x = action_taken,
           y = count,
           fill = occupancy_type)) +
  
  geom_col(position = "dodge") +
  
  # LABELS ONLY for Principal residence (clean and readable)
  geom_text(
    data = subset(plot_6d_data, occupancy_type == "Principal residence"),
    aes(label = comma(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 4,
    fontface = "bold",
    color = "black"
  ) +
  
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.15))) +
  
  scale_fill_manual(
    values = c(
      "Principal residence" = "#1F78B4",
      "Second residence"    = "#33A02C",
      "Investment property" = "#E31A1C"
    ),
    name = "Occupancy Type"
  ) +
  
  labs(
    title = "Counts of Action Taken by Occupancy Type",
    subtitle = "No observations fall under 'Not Applicable' (0% of dataset)",
    x = "Action Taken",
    y = "Count of Loans"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 19),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "right",
    plot.margin = margin(15, 25, 15, 10)
  )
