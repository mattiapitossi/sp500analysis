library(ggplot2)
library(dplyr)
library(patchwork)

# Load the data into R
sp500 <- read.csv("sp500_index.csv", header = TRUE, sep = ",")
msciworld <- read.csv("MSCI.csv", header = TRUE, sep = ",")

# Convert Date column to Date format
sp500$Date <- as.Date(sp500$Date)
msciworld$Date <- as.Date(msciworld$Date)

# Calculate the percentage change in adjusted closing price from 10 years ago
sp500 <- sp500 %>%
  arrange(Date) %>%
  mutate(Pct_Change = 100 * ((SandP500 / SandP500[1]) - 1))

msciworld <- msciworld %>%
  arrange(Date) %>%
  mutate(Pct_Change = 100 * ((Close / Close[1]) - 1))

perc_chage <- sp500 %>%
  pull(last(Pct_Change))

mgdata <- sp500 %>%
  group_by(Date) %>%
  summarise(avgClose = mean(Pct_Change))

mgdata2 <- msciworld %>%
  group_by(Date) %>%
  summarise(avgClose2 = mean(Pct_Change))

# Create the plot for SP500 index
plot_sp500_index <- ggplot(mgdata, aes(x = Date, y = avgClose)) +
  geom_line(color = "blue") +
  labs(title = "S&P500", x = "Date", y = "avgClose") +
  ggtitle("S&P500")

ggsave("sp500.png", plot_sp500_index, width = 12, height = 6)


# Create the plot for SP500 and MSCI comparison
plot <- ggplot() +
  geom_line(data = mgdata, aes(Date, avgClose, group = 1, color = "S&P500"), label = "S&P500") +
  geom_line(data = mgdata2, aes(Date, avgClose2, group = 1, color = "MSCI World"), label = "MSCI World") +
  labs(color = "Indexes") + 
  scale_color_manual(values = c("red", "blue"), labels = c("MSCI World", "S&P500"))


ggsave("s&p500.png", plot, width = 12, height = 6)
print(plot)
