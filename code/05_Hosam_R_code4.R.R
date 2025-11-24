############################################################
# Question 5(a): Summary statistics of loan amount by
#                state, year, and loan type
############################################################

# Libraries for Q5(a)
library(arrow)
library(dplyr)

# Load loan-level data for Q5
data <- read_parquet("g3lar.parquet")

# Quick structure check (optional)
names(data)
str(data)

# Q5(a): min, max, mean loan amounts
table_5a <- data %>%
  group_by(state_code, activity_year, loan_type) %>%
  summarise(
    min_loan  = min(loan_amount, na.rm = TRUE),
    max_loan  = max(loan_amount, na.rm = TRUE),
    mean_loan = mean(loan_amount, na.rm = TRUE),
    .groups = "drop"
  )

table_5a


############################################################
# Question 5(b): Average loan amount by loan type, state,
#                and year (grouped bar chart)
############################################################

# Libraries for Q5(b)
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
    loan_type_short = fct_relevel(
      loan_type_short,
      "Conventional", "FHA", "VA", "USDA"
    )
  )

# Final plot for Q5(b)
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


############################################################
# Question 5(c): Average interest rate by year and HOEPA
#                status (2021 & 2023 only)
############################################################

# Libraries for Q5(c)
library(dplyr)

table_5c <- data %>% 
  filter(activity_year %in% c(2021, 2023)) %>%  # dataset has only 2021 & 2023
  group_by(activity_year, hoepa_status) %>% 
  summarise(
    avg_interest_rate = mean(interest_rate, na.rm = TRUE),
    .groups = "drop"
  )

table_5c

# Note for Q5(c):
# The dataset only contains years 2021 and 2023 (no records for 2020 or 2022).
# Therefore, the table reports the average interest rate for available years only.


############################################################
# Question 5(d): Ethnicity and action taken
############################################################

# Libraries for Q5(d)
library(dplyr)

# Q5(d)(i): Two-way table of counts only
table_5d <- data %>%
  count(action_taken, derived_ethnicity, name = "count")

table_5d

# Q5(d)(ii): Percentage of records with derived_ethnicity
#            = "Ethnicity Not Available"
pct_ethnicity_not_app <- mean(
  data$derived_ethnicity == "Ethnicity Not Available",
  na.rm = TRUE
) * 100

pct_ethnicity_not_app

# Answer (Q5d-ii): 
# Approximately 25.94% of all observations fall into the "Ethnicity Not Available"
# category. The distribution of missing ethnicity data is not random across
# action_taken categories; it is heavily concentrated in some actions (e.g.,
# 'Loan originated') and much lower in others, indicating that missingness
# is systematic rather than random.


############################################################
# Question 5(e): Two-way table of action taken by age group
############################################################

# Libraries for Q5(e)
library(dplyr)

data_age <- data %>%
  filter(applicant_age != 8888) %>%        # drop invalid age
  mutate(
    age_group = case_when(
      applicant_age < 25 ~ "<25",
      applicant_age >= 25 & applicant_age <= 34 ~ "25–34",
      applicant_age >= 35 & applicant_age <= 44 ~ "35–44",
      applicant_age >= 45 & applicant_age <= 54 ~ "45–54",
      applicant_age >= 55 & applicant_age <= 64 ~ "55–64",
      applicant_age >= 65 & applicant_age <= 74 ~ "65–74",
      applicant_age >= 75 ~ "75+",
      TRUE ~ NA_character_
    ),
    age_group = factor(
      age_group,
      levels = c("<25", "25–34", "35–44", "45–54", "55–64", "65–74", "75+")
    )
  )

table_5e <- data_age %>%
  count(action_taken, age_group, name = "n") %>%
  group_by(action_taken) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%   # ONLY percentages
  select(action_taken, age_group, pct) %>%       # drop counts
  ungroup()

table_5e


############################################################
# Question 18(a): Distribution of median days on market
#                 by state and year (hist + density + normal)
############################################################

# Libraries for Q18(a)
library(arrow)
library(dplyr)
library(ggplot2)
# library(cowplot)       # If you later combine multiple plots

# Load FRED data for Q18
fred_data <- read_parquet("FREDLARTXLA.parquet")

# Quick checks
names(fred_data)
head(fred_data)
table(fred_data$state)
table(fred_data$year)

# Select only records with valid MEDAONMACOUNTY values
q18a_data <- fred_data %>%
  filter(!is.na(MEDAONMACOUNTY))

# Create histogram + density + normal overlay
plot_hist <- ggplot(q18a_data, aes(x = MEDAONMACOUNTY, fill = state)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30, alpha = 0.5, color = "black") +
  geom_density(aes(color = state), size = 1.2) +
  facet_grid(state ~ year) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(q18a_data$MEDAONMACOUNTY, na.rm = TRUE),
      sd   = sd(q18a_data$MEDAONMACOUNTY, na.rm = TRUE)
    ),
    color = "darkred",
    size  = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Distribution of Median Days on Market by State and Year",
    subtitle = "Histogram with Density Curve and Normal Distribution Overlay",
    x = "Median Days on Market",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

plot_hist

# Answer (Q18a):
# The distributions of median days on market differ noticeably by state and year. 
# In both LA and TX, the mean is larger than the median, indicating positive skew
# (a long right tail). The normal curves do not match the histogram/density well:
# the actual distributions are more skewed and have heavier right tails, so they
# are not normally distributed.


############################################################
# Question 18(b): Grouped bar chart of percentage loan
#                 refinancing by state, year, and
#                 homeownership bin
############################################################

# Libraries for Q18(b)
library(dplyr)
library(ggplot2)

# Create homeownership bins
fred_data_binned <- fred_data %>%
  filter(
    !is.na(percentage_loan_refinancing),
    !is.na(HOWNRATEACS)
  ) %>%
  mutate(
    home_bin = case_when(
      HOWNRATEACS < 60 ~ "<60%",
      HOWNRATEACS >= 60 & HOWNRATEACS < 70 ~ "60–70%",
      HOWNRATEACS >= 70 & HOWNRATEACS < 80 ~ "70–80%",
      HOWNRATEACS >= 80 ~ "80%+",
      TRUE ~ NA_character_
    ),
    home_bin = factor(
      home_bin,
      levels = c("<60%", "60–70%", "70–80%", "80%+")
    )
  )

# Aggregate data: average % loan refinancing
q18b_summary <- fred_data_binned %>%
  group_by(state, year, home_bin) %>%
  summarise(
    avg_refi = mean(percentage_loan_refinancing, na.rm = TRUE),
    .groups = "drop"
  )

# Final Enhanced 18(b) Plot
plot_18b <- ggplot(q18b_summary,
                   aes(x = factor(year),
                       y = avg_refi,
                       fill = home_bin)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.6) +
  geom_text(
    aes(label = paste0(round(avg_refi, 1), "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.4,
    size = 3.5,
    color = "black"
  ) +
  facet_wrap(~ state, nrow = 1) +
  scale_fill_brewer(
    palette = "Set2",
    name    = "Homeownership Bin"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title    = "Percentage of Loan Refinancing by State, Year, and Homeownership Group",
    subtitle = "Average share of loans refinanced, grouped by homeownership rate bin",
    x        = "Year",
    y        = "Average % Refinanced"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    # Bold axis titles ONLY
    axis.title.x  = element_text(face = "bold", size = 13, margin = margin(t = 10)),
    axis.title.y  = element_text(face = "bold", size = 13, margin = margin(r = 10)),
    
    # Regular axis tick labels
    axis.text.x   = element_text(size = 11),
    axis.text.y   = element_text(size = 11),
    
    # Main title bold (kept)
    plot.title    = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 8)),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    
    # Facet title bold or plain? (kept bold for clarity)
    strip.text    = element_text(face = "bold", size = 13),
    strip.background = element_rect(fill = "grey90", color = NA),
    
    legend.position = "bottom",
    legend.title    = element_text(face = "bold"),
    legend.text     = element_text(size = 11)
  )

# Show plot
plot_18b

# updated code on Nov 24
