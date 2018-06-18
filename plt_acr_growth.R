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
feb_18 <- read_csv("./Data/crop_acres_state.csv")

###########################################################################################
acres <- filter(feb_18, str_detect(Period, "YEAR$"), State == "LOUISIANA")
f_year <- 2008

# Planted Acres -----------------------------------------
acres$plt_acr <- as.numeric(acres$Value)

growth <- function(cmdy){
  # The growth function takes the variable descriptor for the farm income and wealth
  # statisitcs file and calculates the year to year growth rate of that variable
  # The return command is necessary for the function to output the tibble list
  
  holder <- sub("^([A-Z]+)[[:blank:]].+","\\1",cmdy)
  holder <- tolower(holder)
  holder <- capitalize(holder)
  #holder <- str_c("Planted Acres - ", holder)
  acres_new <- filter(acres, `Data Item` == cmdy, Year >= f_year) %>%
    mutate(grt = log(plt_acr) - log(lead(plt_acr))) %>%
    select(Year, grt)
  names(acres_new) <- gsub("grt", holder[1], names(acres_new), fixed = TRUE)
  return(acres_new)
}

commodities <- c("CORN - ACRES PLANTED", 
                 "SORGHUM - ACRES PLANTED", 
                 "SOYBEANS - ACRES PLANTED",
                 "WHEAT - ACRES PLANTED",
                 "COTTON - ACRES PLANTED") %>%
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

write_csv(g_rate, "./CSV/g_rate_acr_state.csv")