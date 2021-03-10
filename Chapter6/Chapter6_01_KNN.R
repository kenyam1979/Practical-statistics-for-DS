library(tidyverse)
library(FNN)

## Simple KNN 
### Data preparation
loan200 <- read_csv('https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/loan200.csv')
loan200 <-data.frame(loan200) # Note: knn doesn't accept tibble

newloan <- loan200[1, 2:3, drop=FALSE]
knn_pred <- knn(train=loan200[-1, 2:3], test=newloan, cl=loan200[-1, 1], k=20)
knn_pred == 'paid off'  # check prediction result

loan200 %>% 
  ggplot() + geom_point(aes(x=payment_inc_ratio, y=dti, color=outcome))



## KNN and Normalization
### Data preparation
loan_data <- read_csv('https://github.com/gedeck/practical-statistics-for-data-scientists/raw/master/data/loan_data.csv.gz')
loan_data <-data.frame(loan_data)

### Without normalization
loan_df <- model.matrix(~ -1 + payment_inc_ratio + dti + revol_bal + revol_util, data=loan_data)
newloan <- loan_df[1, , drop=FALSE]
loan_df <- loan_df[-1, ]
outcome <- loan_data[-1,]$status
knn_pred <- knn(train=loan_df, test=newloan, cl=outcome, k=5)

loan_df[attr(knn_pred, 'nn.index'),]
newloan[,c('payment_inc_ratio','dti', 'revol_bal','revol_util')]

### With normalization
loan_df <- model.matrix(~ -1 + payment_inc_ratio + dti + revol_bal + revol_util, data=loan_data)
loan_std <- scale(loan_df)
newloan_std <- loan_std[1, , drop=FALSE]
loan_df <- loan_df[-1, ]
loan_std <- loan_std[-1, ]
outcome <- loan_data[-1,]$status
knn_pred <- knn(train=loan_std, test=newloan_std, cl=outcome, k=5)

loan_df[attr(knn_pred, 'nn.index'),]
newloan[,c('payment_inc_ratio','dti', 'revol_bal','revol_util')]


## Probability 
borrow_df <- model.matrix(~ -1 + dti + revol_bal + revol_util + open_acc + delinq_2yrs_zero + pub_rec_zero, data=loan_data)
borrow_knn <- knn(borrow_df, test=borrow_df, cl=loan_data[, 'outcome'], prob=TRUE, k=20)
attr(borrow_knn, 'prob')


rm(list=ls(all.names=TRUE))
