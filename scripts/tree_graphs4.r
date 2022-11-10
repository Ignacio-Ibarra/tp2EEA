library(rattle)
library(rpart)
library(rpart.plot)
library(tidyverse)

set.seed(911)

n = 30

dtrain <- data.frame(x = runif(n,0,2*pi))

dtrain <- dtrain %>% mutate(y = exp(x/3)*cos(x) + rnorm(n,0,0.5) )

dtrain %>% ggplot(aes(x = x, y = y)) +
  geom_point()

tree <- rpart(y ~ x, data = dtrain, method = "anova",maxdepth = 3, minsplit = 1, minbucket = 1, cp = 0)

fancyRpartPlot(tree,digits = 3)

fitted.values <- predict(tree, newdata = dtrain)

dtrain %>% ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_step(aes(x = x, y = fitted.values))

summary(tree)

rpart.rules(tree)
