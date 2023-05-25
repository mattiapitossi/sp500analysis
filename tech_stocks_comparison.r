library(ggplot2)
library(dplyr)
library(RColorBrewer)

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


# Find the ten best performers
top_ten_tech_performers <- sp500 %>%
  group_by(Symbol) %>%
  summarise(Total_Return = last(Pct_Change) - first(Pct_Change)) %>%
  filter(!is.na(Total_Return)) %>%
  arrange(desc(Total_Return)) %>%
  slice(1:10) %>%
  pull(Symbol)

# Find the ten worst performers
worst_ten_tech_performers <- sp500 %>%
  group_by(Symbol) %>%
  summarise(Total_Return = last(Pct_Change) - first(Pct_Change)) %>%
  filter(!is.na(Total_Return)) %>%
  arrange(Total_Return) %>%
  slice(1:10) %>%
  pull(Symbol)

# Top ten performers industry
top_ten_performers_sectors <- sp500_tech_companies %>%
  filter(Symbol %in% top_ten_tech_performers) %>%
  mutate(symbol = Symbol) %>%
  mutate(sector = Sector) %>%
  mutate(sector = Industry) %>%
  select(Symbol, Sector, Industry)

# Worst ten performers industry
worst_ten_performers_sectors <- sp500_tech_companies %>%
  filter(Symbol %in% worst_ten_tech_performers) %>%
  mutate(symbol = Symbol) %>%
  mutate(sector = Sector) %>%
  mutate(sector = Industry) %>%
  select(Symbol, Sector, Industry)

# Set the font size for labels
label_font_size <- 1.5

# Set the radius for the pie chart
pie_radius <- 0.8

# Create a data frame with top industry names and their frequencies
top_industry <- c(select(top_ten_performers_sectors, Industry))
top_industry_counts <- table(top_industry)
top_industry_percentages <- prop.table(top_industry_counts) * 100

# Plot the pie chart of top tech industry
top_labels <- paste(names(top_industry_percentages), "\n", sprintf("%.1f%%", top_industry_percentages), sep="")
png(file="pie_chart_top_tech.png", width=1200, height=800)
pie(top_industry_percentages, labels = top_labels, col = brewer.pal(10,"Set3"), cex = label_font_size, radius = pie_radius)
dev.off()

# Create a data frame with worst industry names and their frequencies
worst_industry <- c(select(worst_ten_performers_sectors, Industry))
worst_industry_counts <- table(worst_industry)
worst_industry_percentages <- prop.table(worst_industry_counts) * 100

# Plot the pie chart of worst industry
worst_labels <- paste(names(worst_industry_percentages), "\n", sprintf("%.1f%%", worst_industry_percentages), sep="")
png(file="pie_chart_worst_tech.png", width=1200, height=800)
pie(worst_industry_percentages, labels = worst_labels, col = brewer.pal(10,"Set3"), cex = label_font_size, radius = pie_radius)
dev.off()


# Print worst ten perfomers
mgdata_worst <- sp500 %>%
  filter(Symbol %in% worst_ten_tech_performers) %>%
  group_by(Date, Symbol) %>%
  summarise(avgClose = mean(Close))

plot_worst <- ggplot(mgdata_worst, aes(Date, avgClose, group = 1)) +
  geom_line(color = "blue") +
  labs(x = "", y = "") +
  facet_wrap(~Symbol, nrow = 2)

ggsave("worst_10_tech_performing_stocks.png", plot_worst, width = 12, height = 6)
print(plot_worst)


# Print top ten perfomers
mgdata_top <- sp500 %>%
  filter(Symbol %in% top_ten_tech_performers) %>%
  group_by(Date, Symbol) %>%
  summarise(avgClose = mean(Close))

plot_top <- ggplot(mgdata_top, aes(Date, avgClose, group = 1)) +
  geom_line(color = "blue") +
  labs(x = "", y = "") +
  facet_wrap(~Symbol, nrow = 2)

ggsave("top_10_tech_performing_stocks.png", plot_top, width = 12, height = 6)
print(plot_top)


# Print worst tech performer
plot_data_worst <- sp500 %>%
  filter(Symbol == worst_tech_performer)

plot_worst_title <- paste0(worst_tech_performer, " Performance over the last 10 years")
plot_worst_filename <- paste0(worst_tech_performer, "_plot.png")

plot_worst_performer <- ggplot(plot_data_worst, aes(x = Date, y = Pct_Change)) +
  geom_line(color = "blue") +
  labs(title = plot_worst_title, x = "", y = "") +
  ggtitle(plot_data_worst$Symbol, "Worst Performer Plot")

ggsave("worst_tech_performing_stock.png", plot_worst_performer, width = 12, height = 6)

# Print top tech performer
plot_data_top <- sp500 %>%
  filter(Symbol == top_tech_performer)

plot_top_title <- paste0(top_tech_performer, " Performance over the last 10 years")
plot_top_filename <- paste0(worst_tech_performer, "_plot.png")

plot_top_performer <- ggplot(plot_data_top, aes(x = Date, y = Pct_Change)) +
  geom_line(color = "blue") +
  labs(title = plot_top_title, x = "", y = "") +
  ggtitle(plot_data_top$Symbol, "Top Performer Plot")

ggsave("top_tech_performing_stock.png", plot_top_performer, width = 12, height = 6)