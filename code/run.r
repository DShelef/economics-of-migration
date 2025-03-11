library(weights)
library(Hmisc)
library(foreign)
library(utils)
library(nlme)
library(lfe)
library(AER)
library(rootSolve)
library(abind)
library(numDeriv)
library(eurostat)
library(tidyverse)

library(Cairo)

library(e1071)
library(numDeriv)

library(statar)

library(fixest)


library(dplyr)
library(tidyr)
library(tools)
library(ggplot2)
library(reshape2)

library(readr)
library(kableExtra)


library(knitr)
library(gridExtra)
library(grid)

library(purrr)
library(broom)

library(nleqslv)
library(BB)
library(stringr)
library(haven)
library(readxl)
setwd("C:\\Users\\ozzyz\\Documents\\University\\Immigration\\economics-of-migration")

IMM_AGE_SEX <- "migr_imm8" # Emigration by age and sex
EM_AGE_SEX <- "migr_emi2" # Immigration by age and sex
MIN_WAGE <- "earn_mw_cur" # Minimum wages
POPULATION <- "demo_pjan" # Population

# imm <- read_excel("data/immigration_eurostat_from_europe.xlsx")
# em <- read_excel("data/emigration_eurostat_from_europe.xlsx")
# wg <- read_excel("data/min_wage_eurostat.xlsx")

# # average minimum wage every year
# wg_long <- wg %>%
#   pivot_longer(
#     cols = -c(CTR, Country),
#     names_to = "Year",
#     values_to = "min_wg"
#   )
# wg_long <- wg_long %>%
#     separate(
#     Year,
#     into = c("Year", "Semester"),
#     sep = "-S"
#   )
# wg_long <- wg_long %>% group_by(CTR, Country, Year) %>% summarise(min_wg = mean(min_wg, na.rm = TRUE))
# avg_min_wg <- wg_long %>% group_by(Year) %>% summarise(avg_min_wg = mean(min_wg, na.rm = TRUE))
# wg_long <- wg_long %>% 
#   left_join(avg_min_wg, by = "Year")
# wg_long <- wg_long %>% mutate(rel_min_wg = min_wg/avg_min_wg)
# view(wg_long)

# # transform immigration data to long format
# imm_long <- imm %>%
#   pivot_longer(
#     cols = -c(CTR, Country),
#     names_to = "Year",
#     values_to = "imm"
#   )
# em_long <- em %>%
#   pivot_longer(
#     cols = -c(CTR, Country),
#     names_to = "Year",
#     values_to = "em"
#   )
# dat <- imm_long %>% 
#   left_join(wg_long, by = c("CTR", "Country", "Year")) %>%
#   left_join(em_long, by = c("CTR", "Country", "Year")) %>%
#   drop_na()

# # Create lagged variables for rel_min_wg
# dat <- dat %>%
#   group_by(Country) %>%
#   arrange(Year) %>%
#   mutate(
#     rel_min_wg_lag1 = lag(rel_min_wg, 1),
#     rel_min_wg_lag2 = lag(rel_min_wg, 2),
#     rel_min_wg_lag3 = lag(rel_min_wg, 3)
#   ) %>%
#   ungroup()
#   # Fill NA values with the mean of the respective column
#   # dat <- dat %>%
#   #   mutate(across(c(rel_min_wg, rel_min_wg_lag1, rel_min_wg_lag2, rel_min_wg_lag3), ~ ifelse(is.na(.), 0, .)))
# # Run the regression
# model_1 <- lm(imm ~ rel_min_wg + rel_min_wg_lag1 + rel_min_wg_lag2 + rel_min_wg_lag3 + Country + Year, data = dat)
# model_2 <- lm(em ~ rel_min_wg + rel_min_wg_lag1 + rel_min_wg_lag2 + rel_min_wg_lag3 + Country + Year, data = dat)
# # Summary of the model
# summary(model_1)
# summary(model_2)

pop <- get_eurostat(POPULATION, time_format = "num", type = "label")
pop <- pop %>% filter(age == "Total", sex == "Total") %>%
  rename(pop = "values")

imm <- get_eurostat(IMM_AGE_SEX, time_format = "num", type = "label")
# Filter and transform the age column
imm <- imm %>%
  # filter(str_detect(age, "^\\d+ years$")) %>%
  # mutate(age = as.numeric(str_remove(age, " years$"))) %>%
  filter(age == "Total") %>%
  rename(imm = "values")
imm <- imm %>% 
  filter(sex == "Males", 
    agedef == "Age in completed years", 
    # age >= 15 & age <= 64, 
    TIME_PERIOD <= 2015)
imm <- imm %>% group_by(geo, TIME_PERIOD) %>% summarise(imm = sum(imm, na.rm = TRUE))

em <- get_eurostat("migr_emi2", time_format = "num", type = "label")
# Filter and transform the age column
em <- em %>%
  # filter(str_detect(age, "^\\d+ years$")) %>%
  # mutate(age = as.numeric(str_remove(age, " years$"))) %>%
  filter(age == "Total") %>%
  rename(em = "values")
em <- em %>% 
  filter(sex == "Males", 
    agedef == "Age in completed years", 
    # age >= 15 & age <= 64, 
    TIME_PERIOD <= 2015)
em <- em %>% group_by(geo, TIME_PERIOD) %>% summarise(em = sum(em, na.rm = TRUE))

wg <- get_eurostat(MIN_WAGE, type = "label")
wg <- wg %>% 
  filter(month(TIME_PERIOD) == 1, currency == "Purchasing Power Standard") %>%
  select(c(geo, TIME_PERIOD, values)) %>%
  mutate(TIME_PERIOD = year(TIME_PERIOD)) %>%
  rename(wg = "values")
# average minimum wage every year
avg_min_wg <- wg %>% group_by(TIME_PERIOD) %>%
  summarise(avg_min_wg = mean(wg, na.rm = TRUE))
wg <- wg %>% left_join(avg_min_wg, by = "TIME_PERIOD")
wg <- wg %>% mutate(rel_min_wg = wg/avg_min_wg)

data <- wg %>%
  left_join(imm, by = c("geo", "TIME_PERIOD")) %>%
  left_join(em, by = c("geo", "TIME_PERIOD")) %>%
  left_join(pop[, c("geo", "TIME_PERIOD", "pop")] , by = c("geo", "TIME_PERIOD"))

sum(is.na(data$imm))
sum(is.na(data$em))
sum(is.na(data$rel_min_wg))

# Create lagged variables for rel_min_wg
data <- data %>%
  group_by(geo) %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    rel_min_wg_lag1 = lag(rel_min_wg, 1),
    rel_min_wg_lag2 = lag(rel_min_wg, 2),
    rel_min_wg_lag3 = lag(rel_min_wg, 3),
    rel_min_wg_lead1 = lead(rel_min_wg, 1),
    rel_min_wg_lead2 = lead(rel_min_wg, 2),
    rel_min_wg_lead3 = lead(rel_min_wg, 3)
  ) %>%
  ungroup()

wg_vars <- c(
  "rel_min_wg_lead3",
  "rel_min_wg_lead2",
  "rel_min_wg_lead1",
  "rel_min_wg",
  "rel_min_wg_lag1",
  "rel_min_wg_lag2",
  "rel_min_wg_lag3"
)
# Fill NA values with the mean of the respective column
# data <- data %>%
#   mutate(across(all_of(wg_vars), ~ ifelse(is.na(.), 0, .)))
# Run the regression
formula_1 <- as.formula(paste("log(imm) ~ geo + TIME_PERIOD + ", paste(wg_vars, collapse = " + ")))
formula_2 <- as.formula(paste("log(em) ~ geo + TIME_PERIOD + ", paste(wg_vars, collapse = " + ")))
model_1 <- lm(formula_1, data = data, weights = pop)
model_2 <- lm(formula_2, data = data, weights = pop)
# Summary of the model
summary(model_1)
summary(model_2)

# Extract coefficients and standard errors
coef_1 <- summary(model_1)$coefficients
coef_2 <- summary(model_2)$coefficients

# Create a data frame for plotting
plot_data <- data.frame(
  Variable = wg_vars,
  Estimate_1 = coef_1[wg_vars, "Estimate"],
  SE_1 = coef_1[wg_vars, "Std. Error"],
  Estimate_2 = coef_2[wg_vars, "Estimate"],
  SE_2 = coef_2[wg_vars, "Std. Error"]
)

# Plot the estimates and standard errors for model 1
# Order plot_data by the order of wg_vars
plot_data <- plot_data %>%
  mutate(Variable = factor(Variable, levels = wg_vars)) %>%
  arrange(Variable)

pdf("output/imm_over_min_wage.pdf", width = 8, height = 6)
# Plot the estimates and standard errors for model 1
ggplot(plot_data, aes(x = Variable, y = Estimate_1)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate_1 - SE_1, ymax = Estimate_1 + SE_1), width = 0.2) +
  labs(title = "Estimates and Standard Errors for Immigration Model",
       x = "Variables",
       y = "Estimate") +
  theme_minimal()
dev.off()

pdf("output/em_over_min_wage.pdf", width = 8, height = 6)
# Plot the estimates and standard errors for model 2
ggplot(plot_data, aes(x = Variable, y = Estimate_2)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate_2 - SE_2, ymax = Estimate_2 + SE_2), width = 0.2) +
  labs(title = "Estimates and Standard Errors for Immigration Model",
       x = "Variables",
       y = "Estimate") +
  theme_minimal()
dev.off()

# Plot immigration over relative minimum wage
ggplot(drop_na(data), aes(x = rel_min_wg, y = imm)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Immigration vs Relative Minimum Wage",
       x = "Relative Minimum Wage",
       y = "Immigration") +
  theme_minimal()

# Plot emigration over relative minimum wage
ggplot(drop_na(data), aes(x = rel_min_wg, y = em)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Emigration vs Relative Minimum Wage",
       x = "Relative Minimum Wage",
       y = "Emigration") +
  theme_minimal()
