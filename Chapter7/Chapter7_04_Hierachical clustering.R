library(tidyverse)

## Data preparation
sp500_px <- read_csv('https://github.com/gedeck/practical-statistics-for-data-scientists/raw/master/data/sp500_data.csv.gz')
syms <- c('GOOGL', 'AMZN', 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
df <- t(sp500_px[sp500_px$X1 >='2005-01-01', syms])

## Hierachical clustering
d <- dist(df)
hcl <- hclust(d)
plot(hcl)

cutree(hcl, k=4)


rm(list=ls(all.names=TRUE))
