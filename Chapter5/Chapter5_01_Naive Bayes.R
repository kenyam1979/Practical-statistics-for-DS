library(tidyverse)
library(klaR)

## Data preparation
loan_data <- read_csv('https://github.com/gedeck/practical-statistics-for-data-scientists/raw/master/data/loan_data.csv.gz')
loan_data$outcome <- factor(loan_data$outcome)

## Naive Bayes
naive_model <- NaiveBayes(outcome ~ purpose_ + home_ + emp_len_, data=na.omit(loan_data))
naive_model$tables # list all P(X|Y)

## Prediction

new_loan <- loan_data[147, c('purpose_', 'home_', 'emp_len_')]
predict(naive_model, new_loan)


rm(list=ls(all.names=TRUE))