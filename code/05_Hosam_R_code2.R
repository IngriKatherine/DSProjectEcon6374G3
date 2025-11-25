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

# Q5(c): Average interest rate by year and (property type)
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

library(dplyr)
library(tidyr)

############################################################
# Q5(e): Two-way table: Action taken by applicant_age
############################################################

# Recode age to make 9999 meaningful

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
      levels = c("<25","25–34","35–44","45–54","55–64","65–74","75+")
    )
  )
table_5e <- data_age %>%
  count(action_taken, age_group, name = "n") %>%
  group_by(action_taken) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%   # ONLY percentages
  select(action_taken, age_group, pct) %>%       # drop counts
  ungroup()
table_5e

library(arrow)

fred_data <- read_parquet("FREDLARTXLA.parquet")
names(fred_data)
head(fred_data)

names(fred_data)
head(fred_data)

# Q18a data prep
q18a_data <- fred_data %>%
  filter(!is.na(MEDAONMACOUNTY))   
library(dplyr)
library(ggplot2)
library(cowplot)
install.packages("cowplot")
table(fred_data$state)
table(fred_data$year)

# Q18(a): Histogram + Density + Normal Curve of Median Days on Market
library(dplyr)
library(ggplot2)
library(cowplot)

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
    args = list(mean = mean(q18a_data$MEDAONMACOUNTY, na.rm = TRUE),
                sd = sd(q18a_data$MEDAONMACOUNTY, na.rm = TRUE)),
    color = "darkred",
    size = 1,
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

# Show plot
plot_hist
# Answer (Q18a):
# Answer (Q18a):
# The distributions of median days on market differ noticeably by state and year. 
# In both LA and TX, the mean is larger than the median, which indicates 
# that the distributions are positively skewed (long right tail).
# The added normal curves do NOT match the histogram or density shapes very well.
# The actual distributions show heavier right tails and a lack of symmetry.
# Therefore, the distributions are not normally distributed.

library(dplyr)
library(ggplot2)
library(cowplot)

# Create homeownership bins
fred_data_binned <- fred_data %>%
  filter(!is.na(percentage_loan_refinancing),
         !is.na(HOWNRATEACS)) %>%
  mutate(
    home_bin = case_when(
      HOWNRATEACS < 60 ~ "<60%",
      HOWNRATEACS >= 60 & HOWNRATEACS < 70 ~ "60–70%",
      HOWNRATEACS >= 70 & HOWNRATEACS < 80 ~ "70–80%",
      HOWNRATEACS >= 80 ~ "80%+",
      TRUE ~ NA_character_
    ),
    home_bin = factor(home_bin, levels = c("<60%", "60–70%", "70–80%", "80%+"))
  )

# Aggregate data
q18b_summary <- fred_data_binned %>%
  group_by(state, year, home_bin) %>%
  summarise(
    avg_refi = mean(percentage_loan_refinancing, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
plot_18b <- ggplot(q18b_summary, aes(x = factor(year), 
                                     y = avg_refi, 
                                     fill = home_bin)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(avg_refi, 1)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3) +
  facet_wrap(~ state) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Percentage of Loan Refinancing by State, Year, and Homeownership Group",
    x = "Year",
    y = "Average % Refinanced",
    fill = "Homeownership Bin"
  ) +
  theme_minimal(base_size = 13)

# Show plot
plot_18b



