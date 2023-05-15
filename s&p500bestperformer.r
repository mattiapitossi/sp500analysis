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
top_performer <- sp500_stocks %>%
  group_by(Symbol) %>%
  summarise(Total_Return = last(Pct_Change)) %>%
  filter(!is.na(Total_Return)) %>%
  arrange(desc(Total_Return)) %>%
  slice(1) %>%
  pull(Symbol)

perc_chage <- sp500 %>%
  pull(last(Pct_Change))

mgdata <- sp500 %>%
  group_by(Date) %>%
  summarise(avgClose = mean(Pct_Change))

mgdata_top <- sp500_stocks %>%
  filter(Symbol %in% top_performer) %>%
  group_by(Date, Symbol) %>%
  summarise(avgClose2 = mean(Pct_Change))

plot <- ggplot() +
  geom_line(data = mgdata, aes(Date, avgClose, group = 1, color = "S&P500"), label = "S&P500") +
  geom_line(data = mgdata_top, aes(Date, avgClose2, group = 1, color = top_performer), label = top_performer) +
  labs(color = "Indexes", x = "Date", y = "Performance") +
  scale_color_manual(values = c("red", "blue"), labels = c(top_performer, "S&P500"))


ggsave("s&p500_best.png", plot, width = 12, height = 6)
print(plot)
