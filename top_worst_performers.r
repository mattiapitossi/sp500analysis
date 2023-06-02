library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Load data
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
  geom_line(color = "blue") +
  labs(x = "", y = "") +
  facet_wrap(~Symbol, nrow = 2)

ggsave("worst_10_performing_stocks.png", plot_worst, width = 12, height = 6)
print(plot_worst)

# Print top ten perfomers
mgdata_top <- sp500 %>%
  filter(Symbol %in% top_ten_performers) %>%
  group_by(Date, Symbol) %>%
  summarise(avgClose = mean(Close))

plot_top <- ggplot(mgdata_top, aes(Date, avgClose, group = 1)) +
  geom_line(color = "blue") +
  labs(x = "", y = "") +
  facet_wrap(~Symbol, nrow = 2)

ggsave("top_10_performing_stocks.png", plot_top, width = 12, height = 6)
print(plot_top)


# Print top performer
plot_data_top <- sp500 %>%
  filter(Symbol == top_performer)

plot_title <- paste0(top_performer, " Performance over the last 10 years")
plot_filename <- paste0(top_performer, "_plot.png")

plot_top_performer <- ggplot(plot_data_top, aes(x = Date, y = Pct_Change)) +
  geom_line(color = "blue") +
  labs(title = plot_title, x = "", y = "") +
  ggtitle(plot_data_top$Symbol, "Top Performer Plot")

ggsave("top_performing_stock.png", plot_top_performer, width = 12, height = 6)


# Print worst performer
plot_data_worst <- sp500 %>%
  filter(Symbol == worst_performer)

plot_title <- paste0(worst_performer, " Performance over the last 10 years")
plot_filename <- paste0(worst_performer, "_plot.png")

plot_worst_performer <- ggplot(plot_data_worst, aes(x = Date, y = Adj.Close)) +
  geom_line(color = "blue") +
  labs(title = plot_title, x = "", y = "") +
  ggtitle(plot_data_worst$Symbol, "Worst Performer Plot")

ggsave("worst_performing_stock.png", plot_worst_performer, width = 12, height = 6)

# Set the font size for labels
label_font_size <- 1.5

# Set the radius for the pie chart
pie_radius <- 0.8

# Create a data frame with top sector names and their frequencies
top_sectors <- c(select(top_ten_performers_sectors, Sector))
top_sector_counts <- table(top_sectors)
top_sector_percentages <- prop.table(top_sector_counts) * 100

# Plot the pie chart of top sectors
top_labels <- paste(names(top_sector_percentages), "\n", sprintf("%.1f%%", top_sector_percentages), sep="")
png(file = "pie_chart_top.png", width = 1000, height = 800)
pie(top_sector_percentages, labels = top_labels, col = brewer.pal(10,"Set3"), cex = label_font_size, radius = pie_radius)
dev.off()


# Create a data frame with worst sector names and their frequencies
# It is not possible to use worst_ten_sectors vector, in the dataset are not present the companies WBD 
# and PAR, so we have to manually add "Media and Entertainment" twice
worst_sectors <- worst_sectors <- c("Industrials", "Utilities", "Healthcare", "Energy", "Consumer Cyclical", 
                   "Communication Services", "Communication Services", "Real Estate", "Media and Entertainment", "Media and Entertainment")
worst_sector_counts <- table(worst_sectors)
worst_sector_percentages <- prop.table(worst_sector_counts) * 100

# Plot the pie chart of worst sectors
worst_labels <- paste(names(worst_sector_percentages), "\n", sprintf("%.1f%%", worst_sector_percentages), sep="")
png(file = "pie_chart_worst.png", width = 1000, height = 800)
pie(worst_sector_percentages, labels = worst_labels, col = brewer.pal(10,"Set3"), cex = label_font_size, radius = pie_radius)
dev.off()
