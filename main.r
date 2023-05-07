library(plyr)
library(ggplot2)
library(reshape2)

setwd("/Users/mattia.pitossi/Documents/dataset_s&p")

data <- read.csv("sp500_stocks.csv", header = TRUE, sep = ",")


mgdata <- ddply(
    subset(data, Date >= "2012-01-01" & Date <= "2022-12-31"),
    "Symbol",
    summarise,
    mean_close = mean(Close),
    max_value = max(Close),
    min_value = min(Close)
)

print(mgdata)


ggplot(mgdata, aes(Symbol, max_value)) + geom_point() +
geom_smooth() + xlab("Symbol") + ylab("Max value") +
ggtitle("All stocks")