library(tidyverse)

sp500_px <- read_csv('https://github.com/gedeck/practical-statistics-for-data-scientists/raw/master/data/sp500_data.csv.gz')

###########################
## PCA
oil_px <- sp500_px[, c('CVX', 'XOM')]
pca <- princomp(oil_px)
pca$loadings

loadings <- pca$loadings
ggplot(data=oil_px, aes(x=CVX, y=XOM)) +
  geom_point(alpha=0.3) + 
  stat_ellipse(type='norm', level=0.99) + 
  geom_abline(intercept =0, slope= loadings[2,1]/loadings[1,1]) +
  geom_abline(intercept =0, slope= loadings[2,2]/loadings[1,2])

###　Screeplot

syms <- c('AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
top_sp <- sp500_px[sp500_px$X1 >='2005-01-01', syms]
sp_pca <- princomp(top_sp)
summary(sp_pca)
screeplot(sp_pca)

### Loading plot

loadings <- as_tibble(sp_pca$loadings[,1:5])
loadings$symbol <- rownames(sp_pca$loadings)
loadings %>% pivot_longer(cols=-symbol, names_to="component", values_to="weight") %>%
  ggplot(aes(x=symbol, y=weight)) +
  geom_bar(stat='identity') +
  facet_grid(component~., scales='free_y')

###########################
## Corresponding analysis
library(ca)
housetasks <- read_csv('https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/housetasks.csv')
rname <- housetasks$Task
housetasks <- housetasks[, 2:5]
rownames(housetasks) <- rname
ca_analysis <- ca(housetasks)
plot(ca_analysis)


###########################
## k-means

df <- sp500_px[sp500_px$X1 >= '2011-01-01', c('XOM', 'CVX')]
km <- kmeans(df, centers = 4)
df$cluster <- factor(km$cluster)

centers <- data.frame(cluster=1:4, km$centers)
ggplot() + 
  geom_point(data=df, aes(x=XOM, y=CVX, color=cluster), alpha=0.3) +
  geom_point(data=centers, aes(x=XOM, y=CVX), size=3)


syms <- c('AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
df <- sp500_px[sp500_px$X1 >='2011-01-01', syms]
km <- kmeans(df, centers = 5, nstart = 10)
km$size

loadings <- as_tibble(t(km$centers))
loadings$symbol <- colnames(km$centers)
loadings %>% pivot_longer(cols=-symbol, names_to="cluster", values_to="mean") %>%
  ggplot(aes(x=symbol, y=mean)) +
  geom_bar(stat='identity') +
  facet_grid(cluster~., scales='free_y')
