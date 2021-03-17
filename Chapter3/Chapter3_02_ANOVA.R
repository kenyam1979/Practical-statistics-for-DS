library(tidyverse)

## ANOVA
four_sessions <- read_csv('https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/four_sessions.csv')
four_sessions %>% 
  ggplot(aes(x=Page, y=Time)) +
  geom_boxplot()
### Permutation test
library(lmPerm)
summary(aovp(Time~Page, data=four_sessions))
### F-stat
summary(aov(Time~Page, data=four_sessions))


rm(list=ls(all.names=TRUE))
