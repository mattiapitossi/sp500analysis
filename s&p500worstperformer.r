library(ggplot2)
library(dplyr)
library(patchwork)

# Load the data into R
sp500 <- read.csv("sp500_index.csv", header = TRUE, sep = ",")
sp500_stocks <- read.csv("sp500_stocks.csv", header = TRUE, sep = ",")

# Convert Date column to Date format
sp500$Date <- as.Date(sp500$Date)

# Calculate the percentage change in adjusted closing price from 10 years ago
sp500 <- sp500 %>%
  arrange(Date) %>%
  mutate(Pct_Change = 100 * ((SandP500 / SandP500[1]) - 1))

# Convert Date column to Date format
sp500_stocks$Date <- as.Date(sp500_stocks$Date)

# Calculate the percentage change in adjusted closing price from 10 years ago
sp500_stocks <- sp500_stocks %>%
  group_by(Symbol) %>%
  arrange(Date) %>%
  filter(Date >= (max(Date) - 3650)) %>%
  mutate(Pct_Change = 100 * ((Adj.Close / Adj.Close[1]) - 1)) %>%
  ungroup()

# Find the top performer
worst_performers <- sp500_stocks %>%
  group_by(Symbol) %>%
  summarise(Total_Return = last(Pct_Change)) %>%
  filter(!is.na(Total_Return)) %>%
  arrange(Total_Return) %>%
  slice(1:10) %>%
  pull(Symbol)

perc_chage <- sp500 %>%
  pull(last(Pct_Change))


mgdata <- sp500 %>%
  arrange(Date) %>%
  mutate(Pct_Change = 100 * ((SandP500 / SandP500[1]) - 1))

mgdata_worst <- sp500_stocks %>%
  filter(Symbol %in% worst_performers) %>%
  group_by(Symbol) %>%
  arrange(Date) %>%
  mutate(Pct_Change2 = 100 * ((Close / Close[1]) - 1))

plot <- ggplot(mgdata_worst, aes(x = Date, y = Pct_Change2, color = Symbol)) +
  geom_line() +
  labs(
    title = "Performance of Worst Stocks",
    x = "",
    y = "",
    color = "Symbol"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))


ggsave("s&p500_worst.png", plot, width = 12, height = 6)
print(plot)
