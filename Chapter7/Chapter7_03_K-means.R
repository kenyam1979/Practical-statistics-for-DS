library(tidyverse)

## Data loading
sp500_px <- read_csv('https://github.com/gedeck/practical-statistics-for-data-scientists/raw/master/data/sp500_data.csv.gz')


df <- sp500_px[sp500_px$X1 >= '2011-01-01', c('XOM', 'CVX')]

## K-means
km <- kmeans(df, centers = 4)

df$cluster <- factor(km$cluster)
centers <- data.frame(cluster=1:4, km$centers)
ggplot() + 
  geom_point(data=df, aes(x=XOM, y=CVX, color=cluster), alpha=0.3) +
  geom_point(data=centers, aes(x=XOM, y=CVX), size=3)

## K-means result analysis
syms <- c('AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
df <- sp500_px[sp500_px$X1 >='2011-01-01', syms]
km <- kmeans(df, centers = 5, nstart = 10)
km$size

## Plotting center of cluster 
loadings <- as_tibble(t(km$centers))
loadings$symbol <- colnames(km$centers)
loadings %>% pivot_longer(cols=-symbol, names_to="cluster", values_to="center") %>%
  ggplot(aes(x=symbol, y=mean)) +
  geom_bar(stat='identity') +
  facet_grid(cluster~., scales='free_y')


rm(list=ls(all.names=TRUE))
