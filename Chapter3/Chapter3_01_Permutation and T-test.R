library(tidyverse)

## Permutation test 
### Session time
session_times <- read_csv('https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/web_page_data.csv')
session_times %>% 
  ggplot(aes(x=Page, y=Time)) +
  geom_boxplot()

mean_diff <- session_times %>%
  group_by(Page) %>%
  summarize(mean=mean(Time)) %>%
  summarize(max(mean)-min(mean)) %>% pull()

set.seed(1234)
perm_fun <- function(x, nA, nB) {
  n <- nA + nB
  idx_b <- sample(1:n, nB)
  idx_a <- setdiff(1:n, idx_b)
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  return(mean_diff)
}

perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
  perm_diffs[i] = perm_fun(session_times$Time, 21, 15)
}
hist(perm_diffs)
abline(v=mean_diff)
mean(perm_diffs > mean_diff)


### Conversion
obs_pct_diff <- 100 * (200/23739 - 182/22588)
conversion <- c(rep(0, 45945), rep(1, 382))
perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
  perm_diffs[i] = 100 * perm_fun(conversion, 23739, 22588)
}
hist(perm_diffs)
abline(v=obs_pct_diff)
mean(perm_diffs > obs_pct_diff) # Almost the same as p-value

### Test for equality of proportions for conversion
prop.test(x=c(200, 182), n=c(23739, 22588), alternative='greater')



## T-test
### Session time
t.test(Time~Page, data=session_times, alternative='less')




