library(ggplot2)
library(dplyr)

# Load the data into R
sp500 <- read.csv("sp500_stocks.csv", header = TRUE, sep = ",")
sp500_companies <- read.csv("sp500_companies.csv", header = TRUE, sep = ",")

# Convert Date column to Date format
sp500$Date <- as.Date(sp500$Date)

# Calculate the percentage change in adjusted closing price from 10 years ago
sp500 <- sp500 %>%
  group_by(Symbol) %>%
  arrange(Date) %>%
  filter(Date >= (max(Date) - 3650)) %>%
  mutate(Pct_Change = 100 * ((Adj.Close / Adj.Close[1]) - 1)) %>%
  ungroup()

# Find the top performer
top_performer <- sp500 %>%
  group_by(Symbol) %>%
  summarise(Total_Return = last(Pct_Change)) %>%
  filter(!is.na(Total_Return)) %>%
  arrange(desc(Total_Return)) %>%
  slice(1) %>%
  pull(Symbol)

# Find the ten best performers
top_ten_performers <- sp500 %>%
  group_by(Symbol) %>%
  summarise(Total_Return = last(Pct_Change) - first(Pct_Change)) %>%
  filter(!is.na(Total_Return)) %>%
  arrange(desc(Total_Return)) %>%
  slice(1:10) %>%
  pull(Symbol)


# Top ten performers sectors
top_ten_performers_sectors <- sp500_companies %>%
  filter(Symbol %in% top_ten_performers) %>%
  mutate(symbol = Symbol) %>%
  mutate(sector = Sector) %>%
  mutate(sector = Industry) %>%
  select(Symbol, Sector, Industry)


# Find the worst performer
worst_performer <- sp500 %>%
  group_by(Symbol) %>%
  summarise(Total_Return = last(Pct_Change) - first(Pct_Change)) %>%
  filter(!is.na(Total_Return)) %>%
  arrange(Total_Return) %>%
  slice(1) %>%
  pull(Symbol)


# Find the ten worst performers
worst_ten_performers <- sp500 %>%
  group_by(Symbol) %>%
  summarise(Total_Return = last(Pct_Change) - first(Pct_Change)) %>%
  filter(!is.na(Total_Return)) %>%
  arrange(Total_Return) %>%
  slice(1:10) %>%
  pull(Symbol)

# Worst ten performers sectors
worst_ten_performers_sectors <- sp500_companies %>%
  filter(Symbol %in% worst_ten_performers) %>%
  mutate(symbol = Symbol) %>%
  mutate(sector = Sector) %>%
  mutate(sector = Industry) %>%
  select(Symbol, Sector, Industry)

# Print worst ten perfomers
mgdata_worst <- sp500 %>%
  filter(Symbol %in% worst_ten_performers) %>%
  group_by(Date, Symbol) %>%
  summarise(avgClose = mean(Close))

plot_worst <- ggplot(mgdata_worst, aes(Date, avgClose, group = 1)) +
  geom_line() +
  facet_wrap(~Symbol, nrow = 2)

ggsave("worst_10_performing_stocks.png", plot_worst, width = 12, height = 6)
print(plot_worst)

# Print top ten perfomers
mgdata_top <- sp500 %>%
  filter(Symbol %in% top_ten_performers) %>%
  group_by(Date, Symbol) %>%
  summarise(avgClose = mean(Close))

plot_top <- ggplot(mgdata_top, aes(Date, avgClose, group = 1)) +
  geom_line() +
  facet_wrap(~Symbol, nrow = 2)

ggsave("top_10_performing_stocks.png", plot_top, width = 12, height = 6)
print(plot_top)
