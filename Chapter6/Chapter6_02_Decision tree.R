library(tidyverse)
library(rpart)
library(rpart.plot)

## Data loading
loan3000 <- read_csv('https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/loan3000.csv')

## Simple tree
loan_tree <- rpart(outcome~borrower_score + payment_inc_ratio, 
                                      data=loan3000, control=rpart.control(cp=0.005))
rpart.plot(loan_tree)


## Check x-val error v. relative error
loan_tree
loan_tree$cptable
loan_tree$variable.importance

plotcp(loan_tree)


## Simpler tree
loan_tree <- rpart(outcome~borrower_score + payment_inc_ratio, 
                   data=loan3000, control=rpart.control(cp=0.015))
rpart.plot(loan_tree)


rm(list=ls(all.names=TRUE))
