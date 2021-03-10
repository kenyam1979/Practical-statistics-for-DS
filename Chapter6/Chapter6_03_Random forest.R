library(tidyverse)
library(randomForest)

## Data loading
loan3000 <- read_csv('https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/loan3000.csv')
loan3000$outcome <- factor(loan3000$outcome)


## Simple random forest
rf <- randomForest(outcome ~ borrower_score + payment_inc_ratio, data=loan3000)
rf

### Error along tree growth
tibble(error_rate=rf$err.rate[,'OOB'], num_tree=1:rf$ntree) %>%
  ggplot(aes(x=num_tree, y=error_rate)) + geom_line()

### Prediction
pred <- predict(rf, prob=TRUE)
bind_cols(loan3000, pred) %>%
  ggplot(aes(x=borrower_score, y=payment_inc_ratio, color=pred, shape=pred)) + 
  geom_point(alpha=0.5)



## Importance
loan_data <- read_csv('https://github.com/gedeck/practical-statistics-for-data-scientists/raw/master/data/loan_data.csv.gz')
loan_data$outcome <- factor(loan_data$outcome)

rf_all <- randomForest(outcome~loan_amnt+term+annual_inc+dti+
                         payment_inc_ratio+revol_bal+revol_util+purpose+home_ownership+delinq_2yrs_zero+
                         pub_rec_zero+open_acc+grade+emp_length+purpose_+
                         home_+emp_len_+borrower_score,
                         data=loan_data, importance=TRUE) # Parameter importance = TRUE

rf_all

varImpPlot(rf_all, type=1)
varImpPlot(rf_all, type=2)

rm(list=ls(all.names=TRUE))
