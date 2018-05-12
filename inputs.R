# Farm Income Forecast Project.
# The existence of smoothing in farm income forecasts

# Pre - Amble ------------------------------
rm(list = ls())
cat("\f")
getwd()

library(tidyverse)
library(purrr)
library(stringr)
library(magrittr)
library(ggplot2)
library(stargazer)
library(strucchange)
library(Hmisc)
library(zoo)

# Data Import ---------------------------------
#setwd('..')
#rent <- read_csv("./Data/rent.csv")
rent <- read_csv("./Data/rent.csv")
wage <- read_csv("./Data/wage.csv")
b_year <- 2012

rent_oh <- filter(rent, State == "OHIO") %>%
  select(Year, State, `RENT, CASH, CROPLAND - EXPENSE, MEASURED IN $ / ACRE  -  <b>VALUE</b>`, `RENT, CASH, PASTURELAND - EXPENSE, MEASURED IN $ / ACRE  -  <b>VALUE</b>`) %>%
  rename(rent = `RENT, CASH, CROPLAND - EXPENSE, MEASURED IN $ / ACRE  -  <b>VALUE</b>`)

rent_oh$rent <- as.numeric(rent_oh$rent)
#rent_oh$Year <- as.numeric(rent_oh$Year)

ggplot(data = rent_oh, aes(x = Year, y = rent)) +
  geom_line() + labs(y = "Cropland Rent Per Acre") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))


wage_cb <- group_by(wage, Year) %>%
  mutate(a_wge = mean(Value)) %>%
  ungroup() %>%
  select(Year,`Data Item`, a_wge) %>%
  filter(Year >= b_year) %>%
  distinct()

ggplot(data = wage_cb, aes(x = Year, y = a_wge)) +
  geom_line() + labs(y = "Hired Labor Wage Rate") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))
