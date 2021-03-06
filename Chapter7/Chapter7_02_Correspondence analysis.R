library(tidyverse)
library(ca)

housetasks <- read_csv('https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/housetasks.csv')

## Preprocessing
## ca package uses rowname for its plot
rname <- housetasks$Task
housetasks <- housetasks[, 2:5]
rownames(housetasks) <- rname

## Correspondence analysis
ca_analysis <- ca(housetasks)

plot(ca_analysis)

rm(list=ls(all.names=TRUE))
