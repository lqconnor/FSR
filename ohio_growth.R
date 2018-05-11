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

# Data Import ---------------------------------
#setwd('..')
setwd("C:/Users/connor.189/Box Sync")
feb_18 <- read_csv("./ERS Farm Income Forecast - Panel & ERS/farm_income_forecast/Data/farmincome_wealthstatisticsdata_february2018.csv")

###########################################################################################
ohio <- filter(feb_18, State == "US")
f_year <- 2008

growth <- function(x){
  g_rate <- filter(ohio, str_detect(VariableDescriptionTotal, x),
                     Year > f_year) %>%
    mutate(grt = log(Amount) - log(lead(Amount)))
}

commodities <- c("Cash receipts value, broilers , all", "Cash receipts value, chicken eggs , all", 
                 "Cash receipts value, hogs , all", "Cash receipts value, dairy products, all",
                 "Cash receipts value, hay , all", "Cash receipts value, greenhouse/nursery , floricultur") %>%
  map(growth)

