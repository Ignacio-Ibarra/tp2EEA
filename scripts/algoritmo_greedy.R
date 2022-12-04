rm( list=ls() )  
gc()  

library(rpart)
require(rpart.plot)
require(ggplot2)
library(tidyverse)
library(gridExtra)
require(rattle)

set.seed(911)

n = 100

sigma = 0.5
mu = 0

dtrain <- data.frame(x = runif(n,0,1))

dtrain <- dtrain %>% mutate(y = 2*x+1,
                            y.noisy = y + rnorm(n,mu,sigma) )


arbol <- rpart(formula = "y.noisy ~ x",
               data = dtrain,
               method = "anova",
               maxdepth = 3,
               minsplit = 1,
               minbucket = 1)

fancyRpartPlot(arbol)

fitted.values <- predict(arbol, newdata = dtrain)

dtrain %>% 
  ggplot()+
  geom_point(aes(x=x, y=y.noisy))+
  geom_smooth(aes(x=x, y=y.noisy), method="lm", se=F)+theme_light()+labs(x="x", y="y")


