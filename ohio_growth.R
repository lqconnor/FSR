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
feb_18 <- read_csv("../ERS Farm Income Forecast - Panel & ERS/farm_income_forecast/Data/farmincome_wealthstatisticsdata_february2018.csv")

###########################################################################################
ohio <- filter(feb_18, State == "US")
f_year <- 2008

growth <- function(x){
  # The growth function takes the variable descriptor for the farm income and wealth
  # statisitcs file and calculates the year to year growth rate of that variable
  # The return command is necessary for the function to output the tibble list
  
  holder <- sub("Cash receipts value, ","\\1", x)
  holder <- sub(" , all","\\1", holder)     # strips the parts of the commodity desription that is the same and keeps only the unique part
  g_rate <- filter(ohio, str_detect(VariableDescriptionTotal, x),
                     Year > f_year) %>%
    mutate(grt = log(Amount) - log(lead(Amount))) %>%
    select(Year, grt)
  names(g_rate) <- gsub("grt", holder[1], names(g_rate), fixed = TRUE)
  return(g_rate)
}

commodities <- c("Cash receipts value, broilers , all", "Cash receipts value, chicken eggs , all", 
                 "Cash receipts value, hogs , all", "Cash receipts value, dairy products, all",
                 "Cash receipts value, hay , all", "Cash receipts value, soybeans , all", 
                 "Cash receipts value, corn , all", "Cash receipts value, wheat , all", 
                 "Cash receipts value, livestock and products , all$") %>%
  map(growth)

# This combines the growth rate columns into one tibble
for (i in 1:length(commodities)) {
  if(i == 1) {
    g_rate <- commodities[[i]]
  } else {
    junk <- commodities[[i]]
    g_rate <- left_join(g_rate, junk, by = "Year")
  }
}

write_csv(g_rate, "./CSV/g_rate.csv")