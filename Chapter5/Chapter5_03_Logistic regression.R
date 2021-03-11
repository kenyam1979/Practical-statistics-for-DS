library(tidyverse)


## Data preparation
loan_data <- read_csv('https://github.com/gedeck/practical-statistics-for-data-scientists/raw/master/data/loan_data.csv.gz')
loan_data$outcome <- factor(loan_data$outcome, levels=c('paid off', 'default')) # levels can control which could be 0 and 1



## Logistic regression
logistic_model <- glm(outcome ~ payment_inc_ratio + purpose_ + home_ + emp_len_ + borrower_score,
                       data=loan_data, family='binomial')
summary(logistic_model)


## Calculation of probability (predict extracts Y as log(odds), not probability)
pred <- predict(logistic_model)
prob <- 1/(1+exp(-pred))
prob


## Additive model (GAM)
library(mgcv)

logistic_gam <- gam(outcome ~ s(payment_inc_ratio) + purpose_ + home_ + emp_len_ + s(borrower_score),
                    data=loan_data, family='binomial')
summary(logistic_gam)


## Model evaluation
pred <- predict(logistic_gam)
pred_y <- as.numeric(pred > 0)
y <- as.numeric(loan_data$outcome == 'default')

### Confusion matrix
tab <- table(y, pred_y)
tab

### Precision
tab[2, 2]/sum(tab[ ,2])
### Sensitiviy
tab[2, 2]/sum(tab[2, ])
### Specificity
tab[1, 1]/sum(tab[2, ])
### Accuracy
sum(diag(tab))/sum(tab)

### ROC
library(ROCR)
prob <- 1/(1+exp(-pred))
pf <- performance(prediction(as.numeric(prob), y), 'tpr', 'fpr')
plot(pf)

### AUC
tmp <- performance(prediction(as.numeric(prob), y), 'auc')
tmp@y.values    



rm(list=ls(all.names=TRUE))
