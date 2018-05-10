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
setwd('..')
setwd("C:/Users/connor.189/Box Sync")
feb_18 <- read_csv("./ERS Farm Income Forecast - Panel & ERS/farm_income_forecast/Data/farmincome_wealthstatisticsdata_february2018.csv")

###########################################################################################
ohio <- filter(feb_18, State == "US")

expenses <- filter(ohio, str_detect(VariableDescriptionTotal, "Production expenses, operating expenses, excl"),
                   Year > 1980)
ggplot(data = expenses, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Expenses")

fertilizer <- filter(ohio, str_detect(VariableDescriptionTotal, "fertilizer"),
                     Year > 1980)
ggplot(data = fertilizer, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Fertilizer")

pest <- filter(ohio, str_detect(VariableDescriptionTotal, "pesticide"),
                     Year > 1980)
ggplot(data = pest, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Pesticide")

seed <- filter(ohio, str_detect(VariableDescriptionTotal, "Intermediate product expenses, seed"),
               Year > 1980)
ggplot(data = seed, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Seed")

oil <- filter(ohio, str_detect(VariableDescriptionTotal, "Intermediate product expenses, petroleum fuel & oil"),
               Year > 1980)
ggplot(data = oil, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Oil")

labor <- filter(ohio, str_detect(VariableDescriptionTotal, "Labor expenses, all, contract and hired labor expenses"),
              Year > 1980)
ggplot(data = labor, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Hired Labor")

interest <- filter(ohio, str_detect(VariableDescriptionTotal, "Interest expenses, nonreal estate, all"),
              Year > 1980)
ggplot(data = interest, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Interest Payments")

cash <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, all commodities , all"),
               Year > 1980)
ggplot(data = cash, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "All Cash Receipts")

receipts <- c("Cash receipts value, crops , all", "Cash receipts value, livestock and products , all$")
cash_comp <- filter(ohio, str_detect(VariableDescriptionTotal, paste(receipts, collapse = "|")),
               Year == 2016) %>%
  mutate(n_amount = Amount/sum(Amount))
ggplot(data = cash_comp, aes(x = "", y = n_amount, fill = VariableDescriptionTotal)) +
  geom_bar(width = 1, stat = "identity")

receipts <- c("Cash receipts value, corn , all", "Cash receipts value, soybeans , all")
cash_grn <- filter(ohio, str_detect(VariableDescriptionTotal, paste(receipts, collapse = "|")),
                    Year == 2016) %>%
  mutate(n_amount = Amount/sum(Amount))
ggplot(data = cash_grn, aes(x = "", y = n_amount, fill = VariableDescriptionTotal)) +
  geom_bar(width = 1, stat = "identity")

cash_crn <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, corn , all"),
                   Year > 1980)
ggplot(data = cash_crn, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Corn")

cash_soy <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, soybeans , all"),
                   Year > 1980)
ggplot(data = cash_soy, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Soybeans")

cash_dary <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, dairy products, all"),
                   Year > 1980)
ggplot(data = cash_dary, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Dairy")

cash_lvs <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, livestock and products , all$"),
                    Year > 1980)
ggplot(data = cash_lvs, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Livestock")

rent <- filter(ohio, str_detect(VariableDescriptionTotal, "Other farm income, net rent, excluding share rent"),
               Year > 1980)
ggplot(data = rent, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Rent")