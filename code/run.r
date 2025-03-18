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
IMM_AGE_GROUP_SEX_CITIZENSHIP <- "migr_imm1ctz"
EM_AGE_GROUP_SEX_CITIZENSHIP <- "migr_emi1ctz"
GDP_PER_CAPITA <- "sdg_08_10"

schengen <- read_excel("data/schengen_date.xlsx")
# Add Schengen dummy variable
schengen <- schengen %>% 
  mutate(schengen_year = year(Schengen_Date)) %>%
  mutate(schengen_year = ifelse(is.na(schengen_year), Inf, schengen_year))

pop <- get_eurostat(POPULATION, time_format = "num", type = "label")
pop <- pop %>% filter(age == "Total", sex == "Total") %>%
  rename(pop = "values")
wg_base <- get_eurostat(MIN_WAGE, type = "label")
wg_base <- wg_base %>% 
  filter(month(TIME_PERIOD) == 1, currency == "Purchasing Power Standard") %>%
  select(c(geo, TIME_PERIOD, values)) %>%
  mutate(TIME_PERIOD = year(TIME_PERIOD)) %>%
  rename(wg = "values")
# average minimum wage every year
avg_min_wg_base <- wg_base %>% group_by(TIME_PERIOD) %>%
  summarise(avg_min_wg = mean(wg, na.rm = TRUE))
wg_base <- wg_base %>% left_join(avg_min_wg_base, by = "TIME_PERIOD")
wg_base <- wg_base %>% mutate(rel_min_wg = wg/avg_min_wg)
wg_base$wg <- ifelse(is.na(wg_base$wg), 0, wg_base$wg)
wg_base$rel_min_wg <- ifelse(is.na(wg_base$rel_min_wg), 0, wg_base$rel_min_wg)

gdp <- get_eurostat(GDP_PER_CAPITA, time_format = "num", type = "label")
gdp <- gdp %>% filter(
  freq == "Annual", 
  unit == "Chain linked volumes (2020), euro per capita",
  na_item == "Gross domestic product at market prices"
) %>%
  rename(gdp = "values")

migrant_stocks <- read_excel("data/migrant_stocks.xlsx")
migrant_stocks <- migrant_stocks %>%
  pivot_longer(
    cols = -geo,
    names_to = "TIME_PERIOD",
    values_to = "migrant_stock"
  ) %>%
  mutate(TIME_PERIOD = as.numeric(TIME_PERIOD))
# Function to linearly interpolate missing years
interpolate_years <- function(df, value_col) {
  df %>%
    group_by(geo) %>%
    complete(TIME_PERIOD = full_seq(TIME_PERIOD, 1)) %>%
    arrange(geo, TIME_PERIOD) %>%
    mutate(!!value_col := zoo::na.approx(!!sym(value_col), na.rm = FALSE)) %>%
    ungroup()
}

migrant_stocks <- interpolate_years(migrant_stocks, "migrant_stock")

migration_over_rel_min_wg <- function(wg, imm, em, title, use_schengen_dummy = TRUE){
  data <- wg %>%
    left_join(imm, by = c("geo", "TIME_PERIOD")) %>%
    left_join(em, by = c("geo", "TIME_PERIOD")) %>%
    left_join(pop[, c("geo", "TIME_PERIOD", "pop")] , by = c("geo", "TIME_PERIOD")) %>%
    left_join(gdp[, c("geo", "TIME_PERIOD", "gdp")], by = c("geo", "TIME_PERIOD")) %>%
    left_join(migrant_stocks, by = c("geo", "TIME_PERIOD"))
  if(use_schengen_dummy){
  data <- data %>%
    left_join(schengen[, c("geo", "schengen_year")], by = "geo") %>%
    mutate(schengen_dummy = ifelse(TIME_PERIOD > schengen_year, 1, 0))
  }
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
      rel_min_wg_lead3 = lead(rel_min_wg, 3),
      wg_lag1 = lag(wg, 1),
      wg_lag2 = lag(wg, 2),
      wg_lag3 = lag(wg, 3),
      wg_lead1 = lead(wg, 1),
      wg_lead2 = lead(wg, 2),
      wg_lead3 = lead(wg, 3)
    ) %>%
    ungroup()

  # wg_vars <- c(
  #   "rel_min_wg_lead3",
  #   "rel_min_wg_lead2",
  #   "rel_min_wg_lead1",
  #   "rel_min_wg",
  #   "rel_min_wg_lag1",
  #   "rel_min_wg_lag2",
  #   "rel_min_wg_lag3"
  # )
  wg_vars <- c(
    "wg_lead3",
    "wg_lead2",
    "wg_lead1",
    "wg",
    "wg_lag1",
    "wg_lag2",
    "wg_lag3"
  )
  # Fill missing values for lag variables with the closest value after it
  for (var in wg_vars[grep("lag", wg_vars)]) {
    data <- data %>%
      group_by(geo) %>%
      mutate(!!sym(var) := zoo::na.locf(!!sym(var), fromLast = TRUE, na.rm = FALSE)) %>%
      ungroup()
  }

  # Fill missing values for lead variables with the closest value before it
  for (var in wg_vars[grep("lead", wg_vars)]) {
    data <- data %>%
      group_by(geo) %>%
      mutate(!!sym(var) := zoo::na.locf(!!sym(var), na.rm = FALSE)) %>%
      ungroup()
  }
  if(use_schengen_dummy){
    formula_1 <- as.formula(
      paste("log(imm) ~ geo + factor(TIME_PERIOD) * schengen_dummy + gdp + migrant_stock + ",
      paste(wg_vars, collapse = " + ")))
    formula_2 <- as.formula(
      paste("log(em) ~ geo + factor(TIME_PERIOD) * schengen_dummy + gdp + migrant_stock + ",
      paste(wg_vars, collapse = " + ")))
  } else {
    formula_1 <- as.formula(
      paste("log(imm) ~ geo + factor(TIME_PERIOD) + gdp + migrant_stock + ",
      paste(wg_vars, collapse = " + ")))
    formula_2 <- as.formula(
      paste("log(em) ~ geo + factor(TIME_PERIOD) + gdp + migrant_stock + ",
      paste(wg_vars, collapse = " + ")))
  }
  model_1 <- lm(formula_1, data = data, weights = pop)
  model_2 <- lm(formula_2, data = data, weights = pop)
  # Summary of the model
  print(summary(model_1))
  print(summary(model_2))

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

  # pdf(paste0("output/imm_over_min_wage_", title, ".pdf"), width = 8, height = 6)
  png(paste0("output/imm_over_min_wage_", title, ".png"), width = 8, height = 6, units = "in", res = 300)
  # Plot the estimates and standard errors for model 1
  g <- ggplot(plot_data, aes(x = Variable, y = Estimate_1)) +
    geom_point() +
    geom_errorbar(aes(ymin = Estimate_1 - SE_1, ymax = Estimate_1 + SE_1), width = 0.2) +
    labs(title = paste("Immigration", title),
        x = "Variables",
        y = "Estimate") +
      # ylim(c(-4, 4)) +
    theme_minimal()
  print(g)
  dev.off()

  # pdf(paste0("output/em_over_min_wage_", title, ".pdf"), width = 8, height = 6)
  png(paste0("output/em_over_min_wage_", title, ".png"), width = 8, height = 6, units = "in", res = 300)
  # Plot the estimates and standard errors for model 2
  g <- ggplot(plot_data, aes(x = Variable, y = Estimate_2)) +
    geom_point() +
    geom_errorbar(
      aes(ymin = Estimate_2 - SE_2, ymax = Estimate_2 + SE_2), width = 0.2
    ) +
    # ylim(c(-4, 4)) + 
    labs(title = paste("Emigration", title),
        x = "Variables",
        y = "Estimate") +
    theme_minimal()
  print(g)
  dev.off()
}

imm_base <- get_eurostat(IMM_AGE_SEX, time_format = "num", type = "label") %>%
  filter(age == "Total", agedef == "Age in completed years") %>%
  rename(imm = "values")
em_base <- get_eurostat("migr_emi2", time_format = "num", type = "label") %>%
  filter(age == "Total", agedef == "Age in completed years") %>%
  rename(em = "values")

imm_total <- imm_base %>% 
  filter(TIME_PERIOD <= 2015) %>%
  group_by(geo, TIME_PERIOD) %>%
  summarise(imm = sum(imm, na.rm = TRUE))
em_total <- em_base %>% 
  filter(TIME_PERIOD <= 2015) %>%
  group_by(geo, TIME_PERIOD) %>% 
  summarise(em = sum(em, na.rm = TRUE))
migration_over_rel_min_wg(wg_base, imm_total, em_total, "total")

imm_males <- imm_base %>% 
  filter(sex == "Males", TIME_PERIOD <= 2015)
em_males <- em_base %>%
  filter(sex == "Males", TIME_PERIOD <= 2015)
migration_over_rel_min_wg(wg_base, imm_males, em_males, "males")

imm_females <- imm_base %>% 
  filter(sex == "Females", TIME_PERIOD <= 2015)
em_females <- em_base %>%
  filter(sex == "Females", TIME_PERIOD <= 2015)
migration_over_rel_min_wg(wg_base, imm_females, em_females, "females")

schengen_2001 <- schengen %>% 
  filter(schengen_year <= 2001) %>%
  select(geo)
imm_citizenship <- get_eurostat(IMM_AGE_GROUP_SEX_CITIZENSHIP, time_format = "num", type = "label")
em_citizenship <- get_eurostat(EM_AGE_GROUP_SEX_CITIZENSHIP, time_format = "num", type = "label")

wg_schengen <- wg_base %>% 
  filter(geo %in% schengen_2001$geo)
avg_min_wg_schengen <- wg_schengen %>% group_by(TIME_PERIOD) %>%
  summarise(avg_min_wg = mean(wg, na.rm = TRUE)
  )
wg_schengen <- wg_schengen %>% select(-avg_min_wg)
wg_schengen <- wg_schengen %>% left_join(avg_min_wg_schengen, by = "TIME_PERIOD")
wg_schengen <- wg_schengen %>% mutate(rel_min_wg = wg/avg_min_wg)

imm_citizenship_shcengen <- imm_citizenship %>% 
  filter(
    freq == "Annual",
    citizen %in% schengen_2001$geo,
    agedef == "Age in completed years",
    age == "Total",
    unit == "Number",
    sex == "Total",
    geo %in% schengen_2001$geo
  )
imm_citizenship_shcengen <- imm_citizenship_shcengen %>%
  group_by(geo, TIME_PERIOD) %>%
  summarise(imm = sum(values, na.rm = TRUE))

em_citizenship_shcengen <- em_citizenship %>%
  filter(
    freq == "Annual",
    citizen %in% schengen_2001$geo,
    agedef == "Age in completed years",
    age == "Total",
    unit == "Number",
    sex == "Total",
    geo %in% schengen_2001$geo
  )
em_citizenship_shcengen <- em_citizenship_shcengen %>%
  group_by(geo, TIME_PERIOD) %>%
  summarise(em = sum(values, na.rm = TRUE))

migration_over_rel_min_wg(
  wg_schengen,
  imm_citizenship_shcengen,
  em_citizenship_shcengen,
  "citizenship_schengen",
  use_schengen_dummy = FALSE
)
