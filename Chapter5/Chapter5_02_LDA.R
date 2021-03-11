library(tidyverse)
library(MASS)


## Data loading
loan3000 <- read_csv('https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/loan3000.csv')


## LDA (Liner Discriminant Analysis)
loan_lda <- lda(outcome ~ borrower_score + payment_inc_ratio, data=loan3000)
loan_lda$scaling


## Prediction
pred <- predict(loan_lda)
pred$posterior


## Plot
center <- 0.5 * (loan_lda$means[1, ] + loan_lda$means[2, ])
slope <- -loan_lda$scaling[1] / loan_lda$scaling[2]
intercept <- center[2] - center[1] * slope

bind_cols(loan3000,  prob_default=pred$posterior[,1] )%>% 
  ggplot(aes(x=borrower_score, y=payment_inc_ratio, color=prob_default)) +
  geom_point(alpha=0.3) +
  scale_y_continuous(lim=c(0, 20)) + 
  geom_abline(slope=slope, intercept=intercept)



rm(list=ls(all.names=TRUE))