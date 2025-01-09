library(weights)
library(Hmisc)
library(foreign)

library(nlme)
library(lfe)
library(AER)
library(rootSolve)
library(abind)
library(numDeriv)

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

imm <- read_excel("data/immigration_eurostat_from_europe.xlsx")
wg <- read_excel("data/min_wage_eurostat.xlsx")
view(wg)

# average minimum wage every year
wg_long <- wg %>%
  pivot_longer(
    cols = -c(CTR, Country),
    names_to = "Year",
    values_to = "min_wg"
  )
wg_long <- wg_long %>%
    separate(
    Year,
    into = c("Year", "Semester"),
    sep = "-S"
  )
wg_long <- wg_long %>% group_by(CTR, Country, Year) %>% summarise(min_wg = mean(min_wg, na.rm = TRUE))
avg_min_wg <- wg_long %>% group_by(Year) %>% summarise(avg_min_wg = mean(min_wg, na.rm = TRUE))
# 