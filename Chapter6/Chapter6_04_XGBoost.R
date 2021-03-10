library(tidyverse)
library(xgboost)

## Data preparation
loan3000 <- read_csv('https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/loan3000.csv')

predictors <- data.matrix(loan3000[, c('borrower_score', 'payment_inc_ratio')])
label <- as.numeric(loan3000$outcome=='paid off')


## XGBoost
xgb <- xgboost(data=predictors, label=label, objective='binary:logistic',
               params=list(subsample=0.63, eta=0.1), nrounds=100, eval_metric='error')

## Prediction
pred <- predict(xgb, newdata=predictors)
pred_default <- factor(pred >= 0.5)
bind_cols(loan3000, pred_default) %>%
  ggplot(aes(x=borrower_score, y=payment_inc_ratio, color=pred_default, shape=pred_default)) + 
  geom_point(alpha=0.5)


rm(list=ls(all.names=TRUE))
