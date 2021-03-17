library(tidyverse)

## Chi-square test
click_rate <- read_csv('https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/click_rates.csv')
clicks <- matrix(click_rate$Rate, nrow=3, ncol=2, byrow=TRUE)
### Resampling
chisq.test(clicks, simulate.p.value=TRUE)
### Chisq test
chisq.test(clicks, simulate.p.value=FALSE)


## Fisher exact test
fisher.test(clicks)


rm(list=ls(all.names=TRUE))
