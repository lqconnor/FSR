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
wasde <- read_csv("../ERS Farm Income Forecast - Panel & ERS/farm_income_forecast/Data/psd_grains_pulses.csv")
plt_acr <- read_csv("./Data/crop_acres_state.csv")
prices <- read_csv("./Data/prices.csv")
wasde_all <- read_csv("../ERS Farm Income Forecast - Panel & ERS/farm_income_forecast/Data/psd_alldata.csv")
###########################################################################################
ohio <- filter(feb_18, State == "US")
acres <- filter(plt_acr, str_detect(Period, "YEAR$"), Year >= 2000, State == "US TOTAL")
f_year <- 2000

# Receipts ###############################################################################
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

#############################################################################
# Sorghum
sorghum <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, sorghum"))
ggplot(data = sorghum, aes(x = Year, y = Amount)) +
  geom_line() + labs(y = "Sorghum Cash Rceipts") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))

#############################################################################
# Soybeans
soybeans <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, soybeans"))
ggplot(data = soybeans, aes(x = Year, y = Amount)) +
  geom_line() + labs(y = "Soybeans Cash Rceipts") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))

wasde_sgm <- filter(wasde, Commodity_Description == "Sorghum", 
                    Country_Name == "United States", Market_Year >= 2000)

sorghum_price <- filter(prices, State == "US TOTAL", Commodity == "SORGHUM", Period == "MARKETING YEAR") %>%
  select(Year, Commodity, Value)

use_rto <- select(wasde_sgm, Market_Year) %>%
  distinct() %>%
  mutate(use_rto = wasde_sgm$Value[wasde_sgm$Attribute_Description == "Ending Stocks"]/(wasde_sgm$Value[wasde_sgm$Attribute_Description == "Total Supply"] - wasde_sgm$Value[wasde_sgm$Attribute_Description == "Ending Stocks"])) %>%
  left_join(sorghum_price, by = c( "Market_Year" = "Year"))
write_csv(use_rto, "./CSV/use_ratio.csv")

ggplot(data = use_rto) +
  geom_line(aes(x = Market_Year, y = use_rto)) +
  geom_line(aes(x = Market_Year, y = Value, color = "red"))

#######################################################
# Corn
wasde_crn <- filter(wasde, Commodity_Description == "Corn", 
                    Country_Name == "United States", Market_Year >= 2000)

corn_price <- filter(prices, State == "US TOTAL", Commodity == "CORN", Period == "MARKETING YEAR") %>%
  select(Year, Commodity, Value)

use_rto_crn<- select(wasde_crn, Market_Year) %>%
  distinct() %>%
  mutate(use_rto = wasde_crn$Value[wasde_crn$Attribute_Description == "Ending Stocks"]/(wasde_crn$Value[wasde_crn$Attribute_Description == "Total Supply"] - wasde_crn$Value[wasde_crn$Attribute_Description == "Ending Stocks"])) %>%
  left_join(corn_price, by = c( "Market_Year" = "Year"))
write_csv(use_rto_crn, "./CSV/use_ratio_crn.csv")

ggplot(data = use_rto, aes(x = Market_Year, y = use_rto)) +
  geom_line()

# Planted Acres -----------------------------------------
acres$plt_acr <- as.numeric(acres$Value)

plot_acrs <- function(cmdy){
  holder <- sub("^([A-Z]+)[[:blank:]].+","\\1",cmdy)
  holder <- tolower(holder)
  holder <- capitalize(holder)
  holder <- str_c("Planted Acres - ", holder)
  acres_new <- filter(acres, `Data Item` == cmdy, Year >=f_year)
  ggplot(data = acres_new, aes(x = Year, y = plt_acr/1000000)) +
    geom_line() +
    labs(y = holder)
  return(acres_new)
}

#crops <- c("CORN - ACRES PLANTED", "SORGHUM - ACRES PLANTED", "SOYBEANS - ACRES PLANTED")
acres_corn <- plot_acrs("CORN - ACRES PLANTED")
acres_sor <- plot_acrs("SORGHUM - ACRES PLANTED")
acres_soy <- plot_acrs("SOYBEANS - ACRES PLANTED")
#plot_acrs("PEANUTS - ACRES PLANTED")
acres_whe <- plot_acrs("WHEAT - ACRES PLANTED")
acres_cot <- plot_acrs("COTTON - ACRES PLANTED")

ggplot() +
  geom_line(data = acres_corn, aes(x=Year, y = plt_acr/1000000, color = "blue"), size = 1) +
  geom_line(data = acres_cot, aes(x=Year, y = plt_acr/1000000, color = "green"), size = 1) +
  geom_line(data = acres_sor, aes(x=Year, y = plt_acr/1000000, color = "red"), size = 1) +
  geom_line(data = acres_soy, aes(x=Year, y = plt_acr/1000000, color = "orange"), size = 1) +
  geom_line(data = acres_whe, aes(x=Year, y = plt_acr/1000000, color = "purple"), size = 1) +
  scale_color_discrete(name = "Crops", labels = c("Corn", "Cotton", "Sorghum", "Soybeans", "Wheat")) +
  labs(y = "Planted Acres") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=24),
        axis.title=element_text(size=24,face="bold"),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20))
ggsave(filename = "./Plots/plt_acr.tiff", width = 10, height = 8, dpi = 300, units = "in", device='tiff')

take <- function(x) {
  select(x, "Year", "Commodity", "plt_acr")
}

acres_corn <- select(acres_corn, "Year", "Commodity", "plt_acr")
acres_cot <- select(acres_cot, "Year", "Commodity", "plt_acr")
acres_sor <- select(acres_sor, "Year", "Commodity", "plt_acr")
acres_soy <- select(acres_soy, "Year", "Commodity", "plt_acr")
acres_whe <- select(acres_whe, "Year", "Commodity", "plt_acr")

crops <- bind_rows(acres_corn, acres_cot, acres_sor, acres_soy, acres_whe) %>%
  mutate(plt_acr = plt_acr/1000000)
write_csv(crops, "./CSV/acres.csv")

exports_corn <- filter(wasde_all, Attribute_Description == "Exports", 
                       Country_Code == "US", str_detect(Commodity_Description, "Meal, Soybean"),
                       Market_Year >= 2000)

ggplot(data = exports_corn, aes(x = Market_Year, y = Value)) +
  geom_line() + labs(y = "Corn Exports") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))

# FSI as a proportion of cash receipts Corn
exports_corn <- filter(wasde_all, Attribute_Description == "FSI Consumption", 
                       Country_Code == "US", str_detect(Commodity_Description, "Corn"),
                       Market_Year >= 2008) %>%
  left_join(cash_crn, by = c("Market_Year" = "Year")) %>%
  mutate(FSI_rate = Value/Amount)

ggplot(data = exports_corn, aes(x = Market_Year, y = FSI_rate)) +
  geom_line() + labs(y = "Corn Exports") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))

write_csv(exports_corn, "./CSV/FSI_rate_crn.csv")

# FSI as a proportion of cash receipts
exports_sor <- filter(wasde_all, Attribute_Description == "FSI Consumption", 
                       Country_Code == "US", str_detect(Commodity_Description, "Sorghum"),
                       Market_Year >= 2008) %>%
  left_join(sorghum, by = c("Market_Year" = "Year")) %>%
  mutate(FSI_rate = Value/Amount)

ggplot(data = exports_sor, aes(x = Market_Year, y = FSI_rate)) +
  geom_line() + labs(y = "Corn Exports") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))

write_csv(exports_sor, "./CSV/FSI_rate_sor.csv")