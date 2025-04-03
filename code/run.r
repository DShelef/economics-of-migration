library(weights)
library(Hmisc)
library(foreign)
library(utils)
library(nlme)
library(lfe)
library(AER)
library(rootSolve)
library(abind)
library(eurostat)
library(tidyverse)
library(Cairo)
library(tinytex)
library(stargazer)
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
# INCOME_QUANTILES <- "ilc_di03" # median
INCOME_QUANTILES <- "ilc_di01" # Income quantiles

schengen <- read_excel("data/schengen_date.xlsx")
# Add Schengen dummy variable
schengen <- schengen %>% 
  mutate(schengen_year = year(Schengen_Date)) %>%
  mutate(schengen_year = ifelse(is.na(schengen_year), Inf, schengen_year))
eu <- read_excel("data/eu_date.xlsx")
eu <- eu %>% mutate(geo = str_to_title(geo))
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
income <- get_eurostat(INCOME_QUANTILES, time_format = "num", type = "label")
# income <- income %>% filter(
#   freq == "Annual",
#   age == "Total",
#   sex == "Total",
#   indic_il == "Median equivalised net income",
#   unit == "Purchasing power standard (PPS)",
# ) %>%
  # rename(income = "values")
income <- income %>% filter(
  freq == "Annual",
  quantile == "First decile",
  indic_il == "Top cut-off point",
  currency == "Purchasing Power Standard"  
) %>%
  rename(income = "values")


migration_over_rel_min_wg <- function(wg, imm, em, title, use_schengen_dummy = TRUE){
  data <- wg %>%
    left_join(imm, by = c("geo", "TIME_PERIOD")) %>%
    left_join(em, by = c("geo", "TIME_PERIOD")) %>%
    left_join(pop[, c("geo", "TIME_PERIOD", "pop")] , by = c("geo", "TIME_PERIOD")) %>%
    left_join(gdp[, c("geo", "TIME_PERIOD", "gdp")], by = c("geo", "TIME_PERIOD")) %>%
    left_join(migrant_stocks, by = c("geo", "TIME_PERIOD")) %>%
    left_join(income[, c("geo", "TIME_PERIOD", "income")], by = c("geo", "TIME_PERIOD"))
  if(use_schengen_dummy){
    data <- data %>%
      left_join(schengen[, c("geo", "schengen_year")], by = "geo") %>%
      left_join(eu, by = "geo") %>%
      mutate(
        schengen_dummy = ifelse(TIME_PERIOD > schengen_year, 1, 0),
        eu_dummy = ifelse(TIME_PERIOD >= eu_year, 1, 0)
      )
  }
  data$net <- data$imm - data$em
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
  # Create lagged variables for income
  data <- data %>%
    group_by(geo) %>%
    arrange(TIME_PERIOD) %>%
    mutate(
      income_lag1 = lag(income, 1),
      income_lag2 = lag(income, 2),
      income_lag3 = lag(income, 3),
      income_lead1 = lead(income, 1),
      income_lead2 = lead(income, 2),
      income_lead3 = lead(income, 3)
    ) %>%
    ungroup()

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
    formula_imm_no_controls <- as.formula(
      paste("log(imm) ~ geo + factor(TIME_PERIOD) + ",
      paste(wg_vars, collapse = " + ")))
    formula_em_no_controls <- as.formula(
      paste("log(em) ~ geo + factor(TIME_PERIOD) + ",
      paste(wg_vars, collapse = " + ")))
    formula_imm_controls <- as.formula(
      paste("log(imm) ~ geo + factor(TIME_PERIOD) + eu_dummy + schengen_dummy + gdp + migrant_stock + ",
      paste(wg_vars, collapse = " + ")))
    formula_em_controls <- as.formula(
      paste("log(em) ~ geo + factor(TIME_PERIOD) + eu_dummy + schengen_dummy + gdp + migrant_stock + ",
      paste(wg_vars, collapse = " + ")))
    formula_net_no_controls <- as.formula(
      paste("log(net) ~ geo + factor(TIME_PERIOD) + ",
      paste(wg_vars, collapse = " + ")))
    formula_net_controls <- as.formula(
      paste("log(net) ~ geo + factor(TIME_PERIOD) + eu_dummy + schengen_dummy + gdp + migrant_stock + ",
      paste(wg_vars, collapse = " + ")))
  } else {
    formula_imm_no_controls <- as.formula(
      paste("log(imm) ~ geo + factor(TIME_PERIOD) + ",
      paste(wg_vars, collapse = " + ")))
    formula_em_no_controls <- as.formula(
      paste("log(em) ~ geo + factor(TIME_PERIOD) + ",
      paste(wg_vars, collapse = " + ")))
    formula_imm_controls <- as.formula(
      paste("log(imm) ~ geo + factor(TIME_PERIOD) + gdp + migrant_stock + ",
      paste(wg_vars, collapse = " + ")))
    formula_em_controls <- as.formula(
      paste("log(em) ~ geo + factor(TIME_PERIOD) + gdp + migrant_stock + ",
      paste(wg_vars, collapse = " + ")))
    formula_net_no_controls <- as.formula(
      paste("log(net) ~ geo + factor(TIME_PERIOD) + ",
      paste(wg_vars, collapse = " + ")))
    formula_net_controls <- as.formula(
      paste("log(net) ~ geo + factor(TIME_PERIOD) + gdp + migrant_stock + ",
      paste(wg_vars, collapse = " + ")))
  }

# Custom summary function to replace standard errors with robust ones
robust_summary <- function(model) {
  robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
  summary_model <- summary(model)
  summary_model$coefficients[, "Std. Error"] <- robust_se
  summary_model$coefficients[, "t value"] <- summary_model$coefficients[, "Estimate"] / robust_se
  summary_model$coefficients[, "Pr(>|t|)"] <- 2 * pt(-abs(summary_model$coefficients[, "t value"]), df = summary_model$df[2])
  return(summary_model)
}

model_imm_no_controls <- lm(formula_imm_no_controls, data = data, weights = pop)
model_em_no_controls <- lm(formula_em_no_controls, data = data, weights = pop)
model_imm_controls <- lm(formula_imm_controls, data = data, weights = pop)
model_em_controls <- lm(formula_em_controls, data = data, weights = pop)
model_net_no_controls <- lm(formula_net_no_controls, data = data, weights = pop)
model_net_controls <- lm(formula_net_controls, data = data, weights = pop)

# Replace standard errors with robust ones
summary_imm_no_controls <- robust_summary(model_imm_no_controls)
summary_em_no_controls <- robust_summary(model_em_no_controls)
summary_imm_controls <- robust_summary(model_imm_controls)
summary_em_controls <- robust_summary(model_em_controls)
summary_net_no_controls <- robust_summary(model_net_no_controls)
summary_net_controls <- robust_summary(model_net_controls)

  # Create a data frame for plotting
  plot_data <- data.frame(
    Variable = wg_vars,
    Estimate_1 = summary_imm_controls$coefficients[wg_vars, "Estimate"],
    SE_1 = summary_imm_controls$coefficients[wg_vars, "Std. Error"],
    Estimate_2 = summary_em_controls$coefficients[wg_vars, "Estimate"],
    SE_2 = summary_em_controls$coefficients[wg_vars, "Std. Error"],
    Estimate_3 = summary_net_controls$coefficients[wg_vars, "Estimate"],
    SE_3 = summary_net_controls$coefficients[wg_vars, "Std. Error"]
  )

  # Plot the estimates and standard errors for model 1
  # Order plot_data by the order of wg_vars
  plot_data <- plot_data %>%
    mutate(Variable = factor(Variable, levels = wg_vars)) %>%
    arrange(Variable)

  png(paste0("output/imm_over_min_wage_", title, ".png"), width = 8, height = 6, units = "in", res = 300)
  # Plot the estimates and standard errors for model 1
  g <- ggplot(plot_data, aes(x = Variable, y = Estimate_1)) +
    geom_point() +
    geom_errorbar(aes(ymin = Estimate_1 - SE_1, ymax = Estimate_1 + SE_1), width = 0.2) +
    scale_x_discrete(labels = c(
      "wg_lead3" = "-3",
      "wg_lead2" = "-2",
      "wg_lead1" = "-1",
      "wg" = "0",
      "wg_lag1" = "1",
      "wg_lag2" = "2",
      "wg_lag3" = "3"
    )) +
    labs("",
      x = "Time",
      y = "Estimate") +
    theme_minimal()
  print(g)
  dev.off()

  png(paste0("output/em_over_min_wage_", title, ".png"), width = 8, height = 6, units = "in", res = 300)
  # Plot the estimates and standard errors for model 2
  g <- ggplot(plot_data, aes(x = Variable, y = Estimate_2)) +
    geom_point() +
    geom_errorbar(
      aes(ymin = Estimate_2 - SE_2, ymax = Estimate_2 + SE_2), width = 0.2
    ) +
    scale_x_discrete(labels = c(
      "wg_lead3" = "-3",
      "wg_lead2" = "-2",
      "wg_lead1" = "-1",
      "wg" = "0",
      "wg_lag1" = "1",
      "wg_lag2" = "2",
      "wg_lag3" = "3"
    )) +
    labs(title = "",
        x = "Time",
        y = "Estimate") +
    theme_minimal()
  print(g)
  dev.off()

  png(paste0("output/net_over_min_wage_", title, ".png"), width = 8, height = 6, units = "in", res = 300)
  # Plot the estimates and standard errors for model 3
  g <- ggplot(plot_data, aes(x = Variable, y = Estimate_3)) +
    geom_point() +
    geom_errorbar(
      aes(ymin = Estimate_3 - SE_3, ymax = Estimate_3 + SE_3), width = 0.2
    ) +
    scale_x_discrete(labels = c(
      "wg_lead3" = "-3",
      "wg_lead2" = "-2",
      "wg_lead1" = "-1",
      "wg" = "0",
      "wg_lag1" = "1",
      "wg_lag2" = "2",
      "wg_lag3" = "3"
    )) +
    labs(title = "",
        x = "Time",
        y = "Estimate") +
    theme_minimal()
  print(g)
  dev.off()

  m1 <- model_imm_no_controls
  m2 <- model_imm_controls
  m3 <- model_em_no_controls
  m4 <- model_em_controls
  m5 <- model_net_no_controls
  m6 <- model_net_controls
  # Use stargazer to create the table and save it as an HTML file
  
  s1 <- summary_imm_no_controls$coefficients[, "Std. Error"]
  s2 <- summary_imm_controls$coefficients[, "Std. Error"]
  s3 <- summary_em_no_controls$coefficients[, "Std. Error"]
  s4 <- summary_em_controls$coefficients[, "Std. Error"]
  s5 <- summary_net_no_controls$coefficients[, "Std. Error"]
  s6 <- summary_net_controls$coefficients[, "Std. Error"]
# Use stargazer to create the table and save it as an HTML file450
  stargazer(
    m1,
    m2,
    m3,
    m4,
    m5,
    m6,
    type = "html",
    out = paste0("output/immigration_emigration_models_", title, ".html"),
    se = list(s1, s2, s3, s4, s5, s6),
    p.auto = TRUE,
    # omit = c("geo.*", "factor.*", "schengen_dummy", "gdp", "migrant_stock"),
    # omit.labels = c("Country", "Time", "Schengen Member", "GDP", "Migrant Stock"),
    omit = c("geo.*", "factor.*", "gdp", "migrant_stock", "eu_dummy", "schengen_dummy"),
    omit.labels = c("Country", "Time", "GDP", "Migrant Stock", "EU Member", "Schengen Member"),
    omit.yes.no = c("Yes", "No")
  )

  income_vars <- c(
    "income_lead3",
    "income_lead2",
    "income_lead1",
    "income",
    "income_lag1",
    "income_lag2",
    "income_lag3"
  )
  # Fill missing values for lag variables with the closest value after it
  for (var in income_vars[grep("lag", income_vars)]) {
    data <- data %>%
      group_by(geo) %>%
      mutate(!!sym(var) := zoo::na.locf(!!sym(var), fromLast = TRUE, na.rm = FALSE)) %>%
      ungroup()
  }
  # Fill missing values for lead variables with the closest value before it
  for (var in income_vars[grep("lead", income_vars)]) {
    data <- data %>%
      group_by(geo) %>%
      mutate(!!sym(var) := zoo::na.locf(!!sym(var), na.rm = FALSE)) %>%
      ungroup()
  }
  model_imm_income <- lm(
    log(imm) ~ geo + factor(TIME_PERIOD) + migrant_stock + gdp +
      eu_dummy + schengen_dummy +
      income_lead3 + income_lead2 + income_lead1 + income +
      income_lag1 + income_lag2 + income_lag3,
    data = data, weights = pop
  )
  model_em_income <- lm(
    log(em) ~ geo + factor(TIME_PERIOD) + migrant_stock + gdp +
      eu_dummy + schengen_dummy +
      income_lead3 + income_lead2 + income_lead1 + income +
      income_lag1 + income_lag2 + income_lag3,
    data = data, weights = pop
  )
  model_net_income <- lm(
    log(net) ~ geo + factor(TIME_PERIOD) + migrant_stock + gdp +
      eu_dummy + schengen_dummy +
      income_lead3 + income_lead2 + income_lead1 + income +
      income_lag1 + income_lag2 + income_lag3,
    data = data, weights = pop
  )
  # Replace standard errors with robust ones
  summary_imm_income <- robust_summary(model_imm_income)
  summary_em_income <- robust_summary(model_em_income)
  summary_net_income <- robust_summary(model_net_income)
  stargazer(
    model_imm_income,
    model_em_income,
    model_net_income,
    type = "html",
    out = paste0("output/income_models_", title, ".html"),
    se = list(
      summary_imm_income$coefficients[, "Std. Error"],
      summary_em_income$coefficients[, "Std. Error"],
      summary_net_income$coefficients[, "Std. Error"]
    ),
    p.auto = TRUE,
    omit = c("geo.*", "factor.*", "gdp", "migrant_stock", "eu_dummy", "schengen_dummy"),
    omit.labels = c("Country", "Time", "GDP", "Migrant Stock", "EU Member", "Schengen Member"),
    omit.yes.no = c("Yes", "No")
  )
}

imm_base <- get_eurostat(IMM_AGE_SEX, time_format = "num", type = "label") %>%
  filter(age == "Total", 
    agedef == "Age in completed years",
    geo != "European Union - 27 countries (from 2020)") %>%
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
