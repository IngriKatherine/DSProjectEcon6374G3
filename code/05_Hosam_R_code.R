library(arrow)

data <- read_parquet("g3lar.parquet")
names(data)
str(data)
library(dplyr)

table_5a <- data %>%
  group_by(state_code, activity_year, loan_type) %>%
  summarise(
    min_loan  = min(loan_amount, na.rm = TRUE),
    max_loan  = max(loan_amount, na.rm = TRUE),
    mean_loan = mean(loan_amount, na.rm = TRUE),
    .groups = "drop"
  )

table_5a


library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

# Data for Q5(b): drop only "Not Applicable" from the plot
plot_5b_data <- data %>%
  filter(!is.na(loan_type),
         loan_type != "Not Applicable") %>%
  group_by(state_code, activity_year, loan_type) %>%
  summarise(
    avg_loan = mean(loan_amount, na.rm = TRUE),
    .groups = "drop"
  )

# Short, readable loan type labels with a fixed display order
loan_labels <- c(
  "Conventional (not insured or guaranteed by FHA VA RHS or FSA)" = "Conventional",
  "Federal Housing Administration insured (FHA)"                  = "FHA",
  "Veterans Affairs guaranteed (VA)"                             = "VA",
  "USDA RHS or FSA guaranteed"                                   = "USDA"
)

plot_5b_data <- plot_5b_data %>%
  mutate(
    loan_type_short = fct_recode(loan_type, !!!loan_labels),
    loan_type_short = fct_relevel(loan_type_short,
                                  "Conventional", "FHA", "VA", "USDA")
  )

# Final professional plot
plot_5b <- ggplot(plot_5b_data,
                  aes(x = loan_type_short,
                      y = avg_loan,
                      fill = state_code)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = comma(round(avg_loan, 0))),
    position = position_dodge(width = 0.7),
    vjust = -0.35,
    size = 3.7,
    fontface = "bold",
    color = "black"
  ) +
  facet_wrap(~ activity_year, nrow = 1) +
  scale_fill_manual(
    values = c("LA" = "#E64B35", "TX" = "#4DBBD5"),
    name   = "State"
  ) +
  scale_y_continuous(
    labels = dollar_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title    = "Average Loan Amount by Loan Type, State, and Year",
    subtitle = "Loan types: Conventional, FHA, VA, USDA (\"Not Applicable\" excluded from plot only)",
    x        = "Loan Type",
    y        = "Average Loan Amount ($)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 13, margin = margin(b = 8)),
    axis.text.x   = element_text(angle = 20, hjust = 1, vjust = 1),
    axis.title.x  = element_text(margin = margin(t = 10)),
    axis.title.y  = element_text(margin = margin(r = 10)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.text    = element_text(face = "bold", size = 13),
    legend.position = "right",
    legend.title    = element_text(face = "bold")
  )

plot_5b


library(dplyr)

# Q5(c): Average interest rate by year and construction method (property type)
table_5c <- data %>%
  filter(activity_year %in% c(2021, 2023)) %>%   # dataset has 2021 and 2023
  group_by(activity_year, construction_method) %>%
  summarise(
    avg_interest_rate = mean(interest_rate, na.rm = TRUE),
    .groups = "drop"
  )

table_5c


library(dplyr)

# Q5(d)(i): Two-way table of counts only
table_5d <- data %>%
  count(action_taken, derived_ethnicity, name = "count")

table_5d


# Q5(d)(ii): Percentage of records with derived_ethnicity = "Not applicable"
pct_ethnicity_not_app <- mean(data$derived_ethnicity == "Ethnicity Not Available",
                              na.rm = TRUE) * 100
pct_ethnicity_not_app

# Answer (Q5d-ii): 
# Approximately 25.94% of all observations fall into the "Not applicable / Ethnicity Not Available" category.
# No, the distribution of missing ethnicity data is not random across action_taken categories.
# The counts show that "Ethnicity Not Available" is heavily concentrated in certain actions
# such as 'Loan originated' (275,126 cases), and much lower in others.
# This uneven pattern indicates that missing ethnicity is systematic, not random.

