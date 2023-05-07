library(plyr)
library(ggplot2)
library(reshape2)
library(dplyr)

setwd("/Users/mattia.pitossi/vsCodeProjects/sp500analysis/dataset_s&p")

data <- read.csv("sp500_stocks.csv", header = TRUE, sep = ",")

sub <- subset(data, Symbol %in% c("AAPL", "TSLA", "MSFT", "NFLX")
& Date >= "2012-01-01" & Date <= "2022-12-31")


mgdata <- ddply(
    sub,
    ~Date + Symbol,
    summarise,
    avgClose = mean(Close)
)


print(ggplot(mgdata, aes(Date, avgClose, group = 1)) + geom_line() +
       facet_wrap(~Symbol, nrow = 2))