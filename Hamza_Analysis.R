# ============================================================
# Essay: Economic Resilience and the Health-Wealth Gradient:
#        Evidence from US Counties in the Post-Pandemic Era
# ============================================================


# Running this block once to install the packages I need.

install.packages("tidyverse")
install.packages("fixest")
install.packages("modelsummary")
install.packages("scales")
install.packages("broom")


# Loading libraries.
# tidyverse handles data cleaning and figures.
# fixest is better than lm() for models with fixed effects.
# modelsummary produces a clean table comparing all my models.
# broom lets me extract coefficients to plot in Figure 3.

library(tidyverse)
library(fixest)
library(modelsummary)
library(scales)
library(broom)


# Loading the data.
# I downloaded both files from countyhealthrankings.org.
# The main file has income, mortality, and controls.
# The supplemental file has the new Loneliness variable
# which is central to my resilience argument.


main_data <- read_csv("analytic_data2025_v3.csv", skip = 1)
supp_data  <- read_csv("analytic_supplement_20260325.csv")

# skip = 1 because the first row in the CHR file is a label row,
# the actual variable names start from row 2.


# Selecting and renaming variables.
# The raw dataset has nearly 800 columns so I keep only what I need.
# I am also dropping state-level rows (countycode = "000") since
# I want county-level observations only.
# The state variable is reconstructed from the first two digits of
# the FIPS code because the original column was empty after loading.

main_clean <- main_data %>%
  filter(countycode != "000") %>%
  mutate(state = substr(fipscode, 1, 2)) %>%
  select(
    fipscode, state, county,

    # Outcome: Years of Potential Life Lost before age 75
    premature_death   = v001_rawvalue,

    # Main regressor: Median Household Income
    income            = v063_rawvalue,

    # Resilience variables newly available in the 2025 CHR release
    loneliness        = v183_rawvalue,
    access_parks      = v179_rawvalue,
    social_assoc      = v140_rawvalue,

    # Controls for alternative explanations
    uninsured         = v085_rawvalue,
    unemployment      = v023_rawvalue,
    some_college      = v069_rawvalue,
    income_inequality = v044_rawvalue,
    adult_smoking     = v009_rawvalue,
    phys_inactivity   = v070_rawvalue,
    pct_rural         = v058_rawvalue,
    pct_black         = v054_rawvalue,
    pct_hispanic      = v056_rawvalue,
    pct_65plus        = v053_rawvalue,
    population        = v051_rawvalue
  ) %>%
  mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.)))) %>%

  # Log income gives a semi-elasticity interpretation.
  # The coefficient tells me how YPLL changes for a 1% rise in income.
  mutate(
    log_income       = log(income),
    income_thousands = income / 1000
  ) %>%
  filter(!is.na(premature_death), !is.na(income))


# Summary statistics for all key variables.

summary_stats <- main_clean %>%
  select(
    `Premature Death (YPLL/100k)` = premature_death,
    `Median Household Income ($)`  = income,
    `Feelings of Loneliness (%)`   = loneliness,
    `Access to Parks (%)`          = access_parks,
    `Uninsured (%)`                = uninsured,
    `Unemployment (%)`             = unemployment,
    `Some College (%)`             = some_college,
    `Physical Inactivity (%)`      = phys_inactivity,
    `Adult Smoking (%)`            = adult_smoking,
    `% Rural`                      = pct_rural,
    `% Non-Hispanic Black`         = pct_black
  ) %>%
  pivot_longer(everything(), names_to = "Variable") %>%
  group_by(Variable) %>%
  summarise(
    N    = sum(!is.na(value)),
    Mean = round(mean(value, na.rm = TRUE), 2),
    SD   = round(sd(value,   na.rm = TRUE), 2),
    Min  = round(min(value,  na.rm = TRUE), 2),
    Max  = round(max(value,  na.rm = TRUE), 2)
  )

print(summary_stats)


# Loneliness is only available for around 2,364 counties.
# For models that include it, I restrict to complete cases
# so the sample stays consistent across Models 3, 4 and 5.

main_clean2 <- main_clean %>%
  filter(!is.na(loneliness), !is.na(access_parks), !is.na(social_assoc))


# OLS Regression Models.
# I build five models progressively to show how the income coefficient
# changes as I add controls. This addresses omitted variable bias.
# All models use HC1 heteroskedasticity-robust standard errors.

# Model 1: Baseline, just income and premature death
m1 <- feols(premature_death ~ log_income,
            data = main_clean, vcov = "HC1")

# Model 2: Adding demographic and behavioral controls
m2 <- feols(premature_death ~ log_income +
              uninsured + unemployment + some_college +
              adult_smoking + phys_inactivity +
              pct_rural + pct_black + pct_hispanic + pct_65plus,
            data = main_clean, vcov = "HC1")

# Model 3: Adding the resilience variables from my proposal
m3 <- feols(premature_death ~ log_income +
              loneliness + access_parks + social_assoc +
              uninsured + unemployment + some_college +
              adult_smoking + phys_inactivity +
              pct_rural + pct_black + pct_hispanic + pct_65plus,
            data = main_clean2, vcov = "HC1")

# Model 4: Adding state fixed effects.
# This absorbs unobserved state-level differences like Medicaid
# expansion and public health spending that could confound results.
m4 <- feols(premature_death ~ log_income +
              loneliness + access_parks + social_assoc +
              uninsured + unemployment + some_college +
              adult_smoking + phys_inactivity +
              pct_rural + pct_black + pct_hispanic + pct_65plus
            | state,
            data = main_clean2, vcov = "HC1")

# Model 5: Interaction between log income and loneliness.
# My argument is that the income gradient is weaker in lonely counties
# because social isolation reduces the health returns to income.
# This model tests that formally after Figure 2 showed it visually.
m5 <- feols(premature_death ~ log_income * loneliness +
              access_parks + social_assoc +
              uninsured + unemployment + some_college +
              adult_smoking + phys_inactivity +
              pct_rural + pct_black + pct_hispanic + pct_65plus
            | state,
            data = main_clean2, vcov = "HC1")

summary(m4)
summary(m5)


# Regression table comparing all five models.

modelsummary(
  list("(1) Bivariate"   = m1,
       "(2) Controls"    = m2,
       "(3) Resilience"  = m3,
       "(4) State FE"    = m4,
       "(5) Interaction" = m5),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = c(
    log_income              = "Log Median Income",
    loneliness              = "Feelings of Loneliness",
    "log_income:loneliness" = "Log Income x Loneliness",
    access_parks            = "Access to Parks",
    social_assoc            = "Social Associations",
    uninsured               = "Uninsured (%)",
    unemployment            = "Unemployment Rate",
    some_college            = "Some College (%)",
    adult_smoking           = "Adult Smoking (%)",
    phys_inactivity         = "Physical Inactivity (%)",
    pct_rural               = "% Rural",
    pct_black               = "% Non-Hispanic Black",
    pct_hispanic            = "% Hispanic",
    pct_65plus              = "% Aged 65+"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output = "regression_table.txt"
)


# Figure 1: The Income-Mortality Gradient.
# A scatter plot to show the raw negative relationship between
# county income and premature death before any controls are added.

fig1 <- ggplot(main_clean, aes(x = income_thousands, y = premature_death)) +
  geom_point(alpha = 0.25, size = 0.9, colour = "steelblue") +
  geom_smooth(method = "lm", colour = "red", linewidth = 1.2, se = TRUE) +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "k")) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Figure 1: The Health-Wealth Gradient Across US Counties (2025)",
    subtitle = "Each point represents one county; red line = OLS fit",
    x        = "Median Household Income (thousands USD)",
    y        = "Premature Death (YPLL per 100,000)",
    caption  = "Source: 2025 County Health Rankings"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40"),
    panel.grid.minor = element_blank()
  )

ggsave("figure1_scatter_income_death.png", fig1, width = 8, height = 5.5, dpi = 300)


# Figure 2: Loneliness as a Moderator.
# I split counties into loneliness tertiles to check whether
# the income gradient looks different across loneliness levels.
# If loneliness moderates the gradient, the lines should be
# clearly separated, which I then test formally in Model 5.

main_clean2 <- main_clean2 %>%
  mutate(loneliness_group = case_when(
    loneliness <= quantile(loneliness, 0.33, na.rm = TRUE) ~ "Low Loneliness",
    loneliness <= quantile(loneliness, 0.66, na.rm = TRUE) ~ "Medium Loneliness",
    TRUE ~ "High Loneliness"
  )) %>%
  mutate(loneliness_group = factor(loneliness_group,
    levels = c("Low Loneliness", "Medium Loneliness", "High Loneliness")))

fig2 <- ggplot(main_clean2, aes(x = income_thousands, y = premature_death,
                                 colour = loneliness_group)) +
  geom_point(alpha = 0.15, size = 0.7) +
  geom_smooth(method = "lm", linewidth = 1.3, se = FALSE) +
  scale_colour_manual(
    values = c(
      "Low Loneliness"    = "darkgreen",
      "Medium Loneliness" = "orange",
      "High Loneliness"   = "red"),
    name = "Loneliness Level") +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "k")) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Figure 2: Loneliness as a Moderator of the Income-Mortality Gradient",
    subtitle = "Counties grouped into tertiles by Feelings of Loneliness",
    x        = "Median Household Income (thousands USD)",
    y        = "Premature Death (YPLL per 100,000)",
    caption  = "Source: 2025 County Health Rankings"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("figure2_loneliness_moderator.png", fig2, width = 8, height = 5.5, dpi = 300)


# Figure 3: Coefficient Plot.
# I prefer this over a table when comparing effect sizes visually.
# broom::tidy() pulls the estimates from Model 4 into a data frame.
# Red dots are significant at 5%, grey are not.

coef_data <- tidy(m4, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
    log_income      = "Log Median Income",
    loneliness      = "Feelings of Loneliness",
    access_parks    = "Access to Parks",
    social_assoc    = "Social Associations",
    uninsured       = "Uninsured (%)",
    unemployment    = "Unemployment Rate",
    some_college    = "Some College (%)",
    adult_smoking   = "Adult Smoking (%)",
    phys_inactivity = "Physical Inactivity (%)",
    pct_rural       = "% Rural",
    pct_black       = "% Non-Hispanic Black",
    pct_hispanic    = "% Hispanic",
    pct_65plus      = "% Aged 65+"
  )) %>%
  mutate(
    significant = ifelse(p.value < 0.05, "Significant", "Not Significant"),
    term        = fct_reorder(term, estimate)
  )

fig3 <- ggplot(coef_data, aes(x = estimate, y = term, colour = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                 width = 0.3, linewidth = 0.8) +
  geom_point(size = 3) +
  scale_colour_manual(
    values = c("Significant" = "red", "Not Significant" = "grey"),
    name = "") +
  labs(
    title    = "Figure 3: OLS Coefficient Plot - Full Model with State Fixed Effects",
    subtitle = "95% confidence intervals; heteroskedasticity-robust standard errors",
    x        = "Coefficient Estimate (Effect on Premature Death YPLL)",
    y        = NULL,
    caption  = "Source: 2025 County Health Rankings. N = 2,138 counties."
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("figure3_coefficient_plot.png", fig3, width = 8.5, height = 6, dpi = 300)


# Figure 4: Resilience Among Low-Income Counties.
# This is the core figure for my resilience argument.
# I restrict to the bottom income quartile and compare premature death
# between counties with good versus poor social infrastructure.
# If the green box sits lower than the red, social infrastructure
# is acting as a buffer against the health penalty of poverty.

low_income_cutoff <- quantile(main_clean2$income, 0.25, na.rm = TRUE)

fig4_data <- main_clean2 %>%
  filter(income <= low_income_cutoff) %>%
  mutate(
    low_lonely = loneliness   < median(loneliness,   na.rm = TRUE),
    high_parks = access_parks > median(access_parks, na.rm = TRUE),
    resilience = case_when(
      low_lonely & high_parks   ~ "Low Loneliness\n& High Park Access",
      !low_lonely & !high_parks ~ "High Loneliness\n& Low Park Access",
      TRUE ~ "Mixed"
    )
  ) %>%
  filter(resilience != "Mixed")

fig4 <- ggplot(fig4_data,
               aes(x = resilience, y = premature_death, fill = resilience)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.3) +
  scale_fill_manual(
    values = c(
      "Low Loneliness\n& High Park Access"  = "darkgreen",
      "High Loneliness\n& Low Park Access"  = "red"),
    guide = "none") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Figure 4: Resilience Among Low-Income Counties",
    subtitle = "Bottom income quartile only - social infrastructure as a buffer",
    x        = "Social Infrastructure Profile",
    y        = "Premature Death (YPLL per 100,000)",
    caption  = "Source: 2025 County Health Rankings"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40"),
    panel.grid.minor = element_blank()
  )

ggsave("figure4_resilience_boxplot.png", fig4, width = 7, height = 5.5, dpi = 300)
