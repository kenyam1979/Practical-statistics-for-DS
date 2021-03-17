library(tidyverse)

## Data prep
house <- read_tsv('https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv', col_names=FALSE, skip=1)
colnames(house) <- c('Id', 'DocumentDate','SalePrice','PropertyID','PropertyType','ym','zhvi_px','zhvi_idx',
                     'AdjSalePrice','NbrLivingUnits','SqFtLot','SqFtToLiving','SqFtFinBasement','Bathrooms',
                     'Bedrooms','BldgGrade','YrBuilt','YrRenovated','TrafficNoise','LandVal','ImpsVal','ZipCode','NewConstruction')


## Outlier analysis
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
bind_cols(lm_98105$coefficients, lm_98105_2$coefficients) # Comparison





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



## Polynominal regression
lm_poly <- lm(AdjSalePrice ~ poly(SqFtToLiving, 2) + SqFtLot + Bathrooms +
     Bedrooms + BldgGrade,
   data=house_98105)

terms <- predict(lm_poly, type='terms')
partial_resid <- residuals(lm_poly) + terms

bind_cols(SqFtToLiving=house_98105$SqFtToLiving, terms=terms[, 'poly(SqFtToLiving, 2)'], partial_resid=partial_resid[, 'poly(SqFtToLiving, 2)']) %>%
  ggplot(aes(x=SqFtToLiving, y=partial_resid)) + 
  geom_point() + 
  geom_smooth(linetype=2) +
  geom_line(aes(x=SqFtToLiving, y=terms))


## Spline regression
library(splines)
lm_spline <- lm(AdjSalePrice ~ bs(SqFtToLiving, degree=3) + SqFtLot + Bathrooms +
                Bedrooms + BldgGrade,
              data=house_98105)

terms <- predict(lm_spline, type='terms')
partial_resid <- residuals(lm_spline) + terms

bind_cols(SqFtToLiving=house_98105$SqFtToLiving, terms=terms[, 'bs(SqFtToLiving, degree = 3)'], partial_resid=partial_resid[, 'bs(SqFtToLiving, degree = 3)']) %>%
  ggplot(aes(x=SqFtToLiving, y=partial_resid)) + 
  geom_point() + 
  geom_smooth(linetype=2, method='lm', formula=y~poly(x,2)) +
  geom_line(aes(x=SqFtToLiving, y=terms))


## General additive
library(mgcv)
lm_gam <- gam(AdjSalePrice ~ s(SqFtToLiving) + SqFtLot + Bathrooms +
                  Bedrooms + BldgGrade,
                data=house_98105)

terms <- predict(lm_gam, type='terms')
partial_resid <- residuals(lm_gam) + terms

bind_cols(SqFtToLiving=house_98105$SqFtToLiving, terms=terms[, 's(SqFtToLiving)'], partial_resid=partial_resid[, 's(SqFtToLiving)']) %>%
  ggplot(aes(x=SqFtToLiving, y=partial_resid)) + 
  geom_point() + 
  geom_smooth(linetype=2) +
  geom_line(aes(x=SqFtToLiving, y=terms))

sigma(lm_98105)
sigma(lm_poly)
sigma(lm_spline)
sigma(lm_gam)


rm(list=ls(all.names=TRUE))
