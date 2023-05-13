library(ggplot2)
library(dplyr)

# Load the data into R
sp500 <- read.csv("/Users/michelafarruggia/Desktop/sp500_stocks.csv", header = TRUE, sep = ",")

# Convert Date column to Date format
sp500$Date <- as.Date(sp500$Date)

# Calculate the percentage change in adjusted closing price from 10 years ago
sp500 <- sp500 %>%
  group_by(Symbol) %>%
  arrange(Date) %>%
  mutate(Pct_Change = 100 * (Adj.Close / Adj.Close[1] - 1)) %>%
  filter(Date >= (max(Date) - 3650)) %>%
  ungroup()

# Find the top performer
top_performer <- sp500 %>%
  group_by(Symbol) %>%
  summarise(Total_Return = last(Pct_Change) - first(Pct_Change)) %>%
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



cat("Top performer:", top_performer, "\n")
cat("Ten top performers:", paste(top_ten_performers, collapse = ", "), "\n")
cat("Worst performer:", worst_performer, "\n")
cat("Ten worst performers:", paste(worst_ten_performers, collapse = ", "), "\n")


# Loop over the best performers and create separate plots for each stock
for (symbol in top_ten_performers) {
  # Create a plot for the current stock
  plot_data <- sp500 %>%
    filter(Symbol == symbol)
  plot_title <- paste0(symbol, " Performance over the last 10 years")
  plot_filename <- paste0(symbol, "_plot.png")
  plot <- ggplot(plot_data, aes(x = Date, y = Pct_Change)) +
    geom_line(color = "blue") +
    labs(title = plot_title, x = "Date", y = "Percentage Change") +
    ggtitle("Top Performer Plot")

  # Save the plot as a PNG file
  ggsave(plot_filename, plot)

  # Display the plot
  print(plot)
}
