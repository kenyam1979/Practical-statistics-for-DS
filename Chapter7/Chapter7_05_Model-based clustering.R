library(tidyverse)
library(mclust)

## Data preparation
sp500_px <- read_csv('https://github.com/gedeck/practical-statistics-for-data-scientists/raw/master/data/sp500_data.csv.gz')
df <- sp500_px[sp500_px$X1 >='2011-01-01', c('XOM', 'CVX')]

## Model-based clustering
mcl <- Mclust(df)
summary(mcl)

cluster <- factor(predict(mcl)$classification)
ggplot(df, aes(x=XOM, y=CVX, color=cluster, shape=cluster)) +
  geom_point(alpha=0.8)

## Result analysis
summary(mcl, parameters=TRUE)$mean
summary(mcl, parameters=TRUE)$variance

## Model selection
plot(mcl, what='BIC', ask=FALSE)


rm(list=ls(all.names=TRUE))
