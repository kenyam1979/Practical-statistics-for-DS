library(tidyverse)

## Simple regression
lung <- read_csv('https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/LungDisease.csv')

model <- lm(PEFR ~ Exposure, data=lung)
model

fitted <- predict(model)
resid <- residuals(model)

lung %>% 
  ggplot(aes(x=Exposure, y=PEFR)) + 
  geom_point() +
  geom_abline(intercept=model$coefficients[1], slope=model$coefficients[2])


## Multiple regression
house <- read_tsv('https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv', col_names=FALSE, skip=1)
colnames(house) <- c('Id', 'DocumentDate','SalePrice','PropertyID','PropertyType','ym','zhvi_px','zhvi_idx',
                     'AdjSalePrice','NbrLivingUnits','SqFtLot','SqFtToLiving','SqFtFinBasement','Bathrooms',
                     'Bedrooms','BldgGrade','YrBuilt','YrRenovated','TrafficNoise','LandVal','ImpsVal','ZipCode','NewConstruction')

house_lm <- lm(AdjSalePrice ~ SqFtToLiving + SqFtLot + Bathrooms +
                 Bedrooms + BldgGrade,
               data=house, na.action=na.omit)
house_lm


## Statistical evaluation
summary(house_lm)


## Model selection
house_full <- lm(AdjSalePrice ~ SqFtToLiving + SqFtLot + Bathrooms +
                 Bedrooms + BldgGrade + PropertyType + NbrLivingUnits + 
                 SqFtFinBasement + YrBuilt + YrRenovated +
                 NewConstruction,
               data=house, na.action=na.omit)
summary(house_full)

library(MASS)
step_lm <- stepAIC(house_full, direction='both')
detach('package:MASS') # there is conflict of select function


## Weighted regression
library(lubridate)
house$year <- year(house$DocumentDate)
house$weight <- house$year - 2005

house_wt <- lm(AdjSalePrice ~ SqFtToLiving + SqFtLot + Bathrooms +
                 Bedrooms + BldgGrade,
               data=house, na.action=na.omit, weight=weight)

round(bind_cols(house_lm$coefficients, house_wt$coefficients), digits=3)


## Dummy variables
lm(AdjSalePrice ~ SqFtToLiving + SqFtLot + Bathrooms +
     Bedrooms + BldgGrade + PropertyType,
   data=house)

### Grouping of variables with a large number of categories
zip_groups <- house %>% 
  mutate(resid=residuals(house_lm)) %>% 
  group_by(ZipCode) %>%
  summarize(med_resid=median(resid), cnt=n()) %>%
  arrange(med_resid) %>%
  mutate(cum_cnt=cumsum(cnt), ZipGroup=ntile(cum_cnt, 5))
zip_groups$ZipGroup <- factor(zip_groups$ZipGroup)

house <- house %>% 
  left_join(select(zip_groups, ZipCode, ZipGroup), by='ZipCode')


## Model interpretation
### Multi-collinearity
step_lm$coefficients
library(GGally)
house %>%
  filter(Bedrooms < 30) %>%  # Outlier
  select(Bedrooms, SqFtToLiving, SqFtFinBasement, Bathrooms) %>%
  ggpairs() # Many of those are correlated. Then remove variables...
update(step_lm, . ~ . - SqFtToLiving - SqFtFinBasement - Bathrooms)

### Confounding variables
lm(AdjSalePrice ~ SqFtToLiving + SqFtLot + Bathrooms +
     Bedrooms + BldgGrade + PropertyType + ZipGroup,
   data=house) # ZipGroup is a confounding variable

### Interactions
lm(AdjSalePrice ~ SqFtToLiving * ZipGroup + SqFtLot + Bathrooms +
     Bedrooms + BldgGrade + PropertyType,
   data=house)


## Outlier
house_98105 <- house[house$ZipCode == 98105,]
lm_98105 <- lm(AdjSalePrice ~ SqFtToLiving + SqFtLot + Bathrooms +
                 Bedrooms + BldgGrade,
               data=house_98105)
sresid <- rstandard(lm_98105)
hist(sresid)

### Measuring leverage (Hat value and Cook's distance)
sresid <- rstandard(lm_98105)
cooks_D <- cooks.distance(lm_98105) 
hat_values <- hatvalues(lm_98105)

bind_cols(cooks_D=cooks_D, sresid=sresid) %>% ggplot(aes(x=cooks_D, y=sresid)) + geom_point()
bind_cols(hat_values=hat_values, sresid=sresid) %>% ggplot(aes(x=hat_values, y=sresid)) + geom_point()

#### Removing outlier
house_98105_2 <- house_98105 %>% filter(cooks_D <= 0.08)
lm_98105_2 <- lm(AdjSalePrice ~ SqFtToLiving + SqFtLot + Bathrooms +
                 Bedrooms + BldgGrade,
               data=house_98105_2)

#### Comparison before/after
bind_cols(lm_98105$coefficients, lm_98105_2$coefficients)


## Residual analysis

### Heteroskedasticity
bind_cols(resid=residuals(lm_98105), pred=predict(lm_98105)) %>%
  ggplot(aes(x=pred, y=abs(resid))) + geom_point() + geom_smooth()

### Non-normal residual
hist(residuals(lm_98105))
qqnorm(residuals(lm_98105)) + qqline(residuals(lm_98105)) ## qq plot
shapiro.test(residuals(lm_98105)) # Shapiro-Wilk test

### Partial residual

terms <- predict(lm_98105, type='terms')
partial_resid <- residuals(lm_98105) + terms

bind_cols(SqFtToLiving=house_98105$SqFtToLiving, terms=terms[, 'SqFtToLiving'], partial_resid=partial_resid[, 'SqFtToLiving']) %>%
  ggplot(aes(x=SqFtToLiving, y=partial_resid)) + 
  geom_point() + 
  geom_smooth(linetype=2) +
  geom_line(aes(x=SqFtToLiving, y=terms))


rm(list=ls(all.names=TRUE))
