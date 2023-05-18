library(ggplot2)
library(dplyr)

# Load the data into R
sp500 <- read.csv("sp500_stocks.csv", header = TRUE, sep = ",")
sp500_companies <- read.csv("sp500_companies.csv", header = TRUE, sep = ",")

# Subset tech companies
sp500_tech_companies <- subset(sp500_companies, sp500_companies$Sector == "Technology")