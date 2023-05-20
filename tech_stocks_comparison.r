library(ggplot2)
library(dplyr)

# Load the data into R
sp500 <- read.csv("sp500_stocks.csv", header = TRUE, sep = ",")
sp500_companies <- read.csv("sp500_companies.csv", header = TRUE, sep = ",")

# Subset tech companies
sp500_tech_companies <- subset(sp500_companies, sp500_companies$Sector == "Technology")

# Create a vector containing all the tech companies' symbols
symbol_tech_companies <- sp500_tech_companies$Symbol

# Convert Date column to Date format
sp500$Date <- as.Date(sp500$Date)

# Calculate the percentage change in adjusted closing price from 10 years ago
sp500 <- sp500 %>%
  filter(Symbol %in% symbol_tech_companies) %>%
  group_by(Symbol) %>%
  arrange(Date) %>%
  filter(Date >= (max(Date) - 3650)) %>%
  mutate(Pct_Change = 100 * ((Adj.Close / Adj.Close[1]) - 1)) %>%
  ungroup()

# Find the top performer among tech companies
top_tech_performer <- sp500 %>%
  group_by(Symbol) %>%
  summarise(Total_Return = last(Pct_Change)) %>%
  filter(!is.na(Total_Return)) %>%
  arrange(desc(Total_Return)) %>%
  slice(1) %>%
  pull(Symbol)

# Find the top performer among tech companies
worst_tech_performer <- sp500 %>%
  group_by(Symbol) %>%
  summarise(Total_Return = last(Pct_Change)) %>%
  filter(!is.na(Total_Return)) %>%
  arrange(Total_Return) %>%
  slice(1) %>%
  pull(Symbol)
