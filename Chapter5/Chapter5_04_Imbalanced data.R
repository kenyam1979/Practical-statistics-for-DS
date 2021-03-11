library(tidyverse)

## Data preparation
full_train_set <- read_csv('https://github.com/gedeck/practical-statistics-for-data-scientists/raw/master/data/full_train_set.csv.gz')
full_train_set$outcome <- factor(full_train_set$outcome)

## Imbalanced data
full_train_set %>% group_by(outcome) %>% summarize(n())
mean(full_train_set$outcome=='default')


## GLM with imbalanced data
full_model <- glm(outcome ~ payment_inc_ratio + purpose_ + home_ +
                    emp_len_ + dti + revol_bal + revol_util,
                  data=full_train_set, family='binomial')
pred <- predict(full_model)
mean(pred < 0)


## Oversampling (weighted data)
wt <- ifelse(full_train_set$outcome=='default',
             1 / mean(full_train_set$outcome=='default'), 1)
full_model <- glm(outcome ~ payment_inc_ratio + purpose_ + home_ +
                    emp_len_ + dti + revol_bal + revol_util,
                  data=full_train_set, weight=wt, family='quasibinomial')
pred <- predict(full_model)
mean(pred < 0)



rm(list=ls(all.names=TRUE))