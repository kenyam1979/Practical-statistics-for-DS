library(tidyverse)

#####################################
## K-means 
#####################################

## Data preparation
loan_data <- read_csv('https://github.com/gedeck/practical-statistics-for-data-scientists/raw/master/data/loan_data.csv.gz')

defaults <- loan_data[loan_data$outcome=='default',]
df <- defaults[, c('loan_amnt', 'annual_inc', 'revol_bal', 'open_acc', 'dti', 'revol_util')]

## Without scaling
km <- kmeans(df, centers=4, nstart=10)
centers <- data.frame(size=km$size, km$centers)
round(centers, digits=2)

## With scaling
df0 <- scale(df)
summary(df0)
km0 <- kmeans(df0, centers=4, nstart=10)
#centers0 <- data.frame(size=km0$size, km0$centers)
#round(centers0, digits=2)

centers0 <- scale(km0$centers, center=FALSE, scale=1/attr(df0, 'scaled:scale'))
centers0 <- scale(centers0, center=-attr(df0, 'scaled:center'), scale=FALSE)
centers0 <- data.frame(size=km0$size, centers0)
round(centers0, digits=2)

#####################################
## Dominant variable without scale 
## (this case Google and Amazon are)
#####################################

## Data preparation
sp500_px <- read_csv('https://github.com/gedeck/practical-statistics-for-data-scientists/raw/master/data/sp500_data.csv.gz')
syms <- c('GOOGL', 'AMZN', 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
top_sp1 <- sp500_px[sp500_px$X1 >='2005-01-01', syms]

## PCA without scaling
sp_pca1 <- princomp(top_sp1)
summary(sp_pca1)
screeplot(sp_pca1)
round(sp_pca1$loadings[,1:2], 3)


#####################################
## Gower distance
#####################################
library(cluster)

## Data preparation
loan_data <- read_csv('https://github.com/gedeck/practical-statistics-for-data-scientists/raw/master/data/loan_data.csv.gz')
defaults <- loan_data[loan_data$outcome=='default',]
df <- defaults[sample(nrow(defaults), 250), c('dti', 'payment_inc_ratio', 'home_', 'purpose_')]
df$home_ <- factor(df$home_)
df$purpose_ <- factor(df$purpose_)

## Distanc calc with Gower distance
d = daisy(df, metric='gower')

## Hierachical clustering
hcl <- hclust(d)
dnd <- as.dendrogram(hcl)
plot(dnd, leaflab='none')

## Result analysis
dnd_cut <- cut(dnd, h=0.5)
df[labels(dnd_cut$lower[[11]]),]


#####################################
## Dummy variable
#####################################
df <- model.matrix(~ -1 + dti + payment_inc_ratio + home_ + pub_rec_zero, data=defaults) 
df0 <- scale(df)
km0 <- kmeans(df0, centers=4, nstart=10)
centers0 <- scale(km0$centers, center=FALSE, scale=1/attr(df0, 'scaled:scale'))
centers0 <- scale(centers0, center=-attr(df0, 'scaled:center'), scale=FALSE)
round(centers0, digits=2)

rm(list=ls(all.names=TRUE))
