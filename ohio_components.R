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
ohio <- filter(feb_18, State == "OH")
f_year <- 2000

expenses <- filter(ohio, str_detect(VariableDescriptionTotal, "Production expenses, operating expenses, excl"),
                   Year >= f_year)
ggplot(data = expenses, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Expenses") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"))

expenses_all <- filter(ohio, str_detect(VariableDescriptionTotal, "Production expenses, all, excl"),
                   Year >= f_year)
expenses_all <- select(expenses_all, Year, State, VariableDescriptionTotal, Amount)
write_csv(expenses_all, "./CSV/expenses.csv")

ggplot(data = expenses_all, aes(x = Year, y = Amount/1000000)) +
  geom_line() + labs(y = "Total Production Expenses") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=24),
        axis.title=element_text(size=24,face="bold"))
ggsave(filename = "./Plots/expenses.tiff", width = 10, height = 8, dpi = 300, units = "in", device='tiff')

fertilizer <- filter(ohio, str_detect(VariableDescriptionTotal, "fertilizer"),
                     Year >= f_year)
ggplot(data = fertilizer, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Fertilizer")

pest <- filter(ohio, str_detect(VariableDescriptionTotal, "pesticide"),
                     Year >= f_year)
ggplot(data = pest, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Pesticide")

seed <- filter(ohio, str_detect(VariableDescriptionTotal, "Intermediate product expenses, seed"),
               Year >= f_year)
ggplot(data = seed, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Seed")

oil <- filter(ohio, str_detect(VariableDescriptionTotal, "Intermediate product expenses, petroleum fuel & oil"),
               Year >= f_year)
ggplot(data = oil, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Oil")

int_exp <- filter(ohio, str_detect(VariableDescriptionTotal, "Intermediate product expenses, all, excl"),
              Year >= f_year)
ggplot(data = int_exp, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Intermediate Expenses")


labor <- filter(ohio, str_detect(VariableDescriptionTotal, "Labor expenses, all, contract and hired labor expenses"),
              Year >= f_year)
ggplot(data = labor, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Hired Labor") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"))

interest <- filter(ohio, str_detect(VariableDescriptionTotal, "Interest expenses, nonreal estate, all"),
              Year >= f_year)
ggplot(data = interest, aes(x = Year, y = Amount/1000000)) +
  geom_line() + labs(y = "Interest Payments")

cash <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, all commodities , all"),
               Year >= f_year)
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
                   Year >= f_year)
ggplot(data = cash_crn, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Corn") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"))

cash_soy <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, soybeans , all"),
                   Year >= f_year)
ggplot(data = cash_soy, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Soybeans")

cash_wh <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, wheat , all"),
                   Year >= f_year)
ggplot(data = cash_wh, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Wheat")

cash_dary <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, dairy products, all"),
                   Year >= f_year)
ggplot(data = cash_dary, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Dairy")

cash_lvs <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, livestock and products , all$"),
                    Year >= f_year)
ggplot(data = cash_lvs, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Livestock")

rent <- filter(ohio, str_detect(VariableDescriptionTotal, "Other farm income, net rent, excluding share rent"),
               Year >= f_year)
ggplot(data = rent, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Rent")

eggs <- filter(ohio, str_detect(VariableDescriptionTotal, "eggs"),
               Year >= f_year)
ggplot(data = rent, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Rent")