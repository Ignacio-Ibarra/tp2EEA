library(rpart)
library(tidyverse)
library(gridExtra)

set.seed(911)

n = 100

sigma = 1.5
mu = 0

dtrain <- data.frame(x = runif(n,0,2*pi))

dtrain <- dtrain %>% mutate(y = exp(x/3)*cos(x),
                            y.noisy = y + rnorm(n,mu,sigma) )

#max_depth = 3 + minsplit = 1 + minbucket= 1
tree1 <- rpart(y.noisy ~ x, data = dtrain, method = "anova",maxdepth = 3, minsplit = 1, minbucket = 1, cp = 0)
fitted.values1 <- predict(tree1, newdata = dtrain)

dtrain %>% ggplot() +
  geom_point(aes(x = x, y = y.noisy), alpha=0.3)+
  geom_line(aes(x=x, y=y))+
  geom_step(aes(x = x, y = fitted.values1), color="blue")+
  theme_light()+
  labs(title="max_depth=3,minsplit=1,minbucket=1", x="",y="")

fancyRpartPlot(tree1, digits=3)

#=======================

# max_depth = 5, minsplit = 1 , minbucket = 1

tree2 <- rpart(y.noisy  ~ x, data = dtrain, method = "anova",maxdepth = 5, minsplit = 1, minbucket = 1, cp = 0)
fitted.values2 <- predict(tree2, newdata = dtrain)

dtrain %>% ggplot() +
  geom_point(aes(x = x, y = y.noisy), alpha=0.3)+
  geom_line(aes(x=x, y=y))+
  geom_step(aes(x = x, y = fitted.values2), color="red")+
  theme_light()+
  labs(title="max_depth=5,minsplit=1,minbucket=1", x="",y="")

fancyRpartPlot(tree2, digits=3)

#=======================

# max_depth = 5  + minsplit = 10 + minbucket = 1
tree3 <- rpart(y.noisy ~ x, data = dtrain, method = "anova",maxdepth = 5, minsplit = 10, minbucket = 1, cp = 0)
fitted.values3 <- predict(tree3, newdata = dtrain)

dtrain %>% ggplot() +
  geom_point(aes(x = x, y = y.noisy), alpha=0.3)+
  geom_line(aes(x=x, y=y))+
  geom_step(aes(x = x, y = fitted.values3), color="darkgrey")+
  theme_light()+
  labs(title="max_depth=5, minsplit=10, minbucket=1", x="",y="")

fancyRpartPlot(tree3, digits=3)

#=======================

# max_depth = 5  + minsplit = 1 + minbucket = 10

tree4 <- rpart(y.noisy ~ x, data = dtrain, method = "anova",maxdepth = 5, minsplit = 1, minbucket = 10, cp = 0)
fitted.values4 <- predict(tree4, newdata = dtrain)

dtrain %>% ggplot() +
  geom_point(aes(x = x, y = y.noisy), alpha=0.3)+
  geom_line(aes(x=x, y=y))+
  geom_step(aes(x = x, y = fitted.values4), color="darkgrey")+
  theme_light()+
  labs(title="max_depth=5, minsplit=1, minbucket=10", x="",y="")

fancyRpartPlot(tree4, digits=3)

#=======================
#El que se ve lindo
# max_depth = 4  + minsplit = 20 + minbucket = 10
tree3 <- rpart(y.noisy ~ x, data = dtrain, method = "anova",maxdepth = 4, minsplit = 20, minbucket = 10, cp = 0)
fitted.values3 <- predict(tree3, newdata = dtrain)

dtrain %>% ggplot() +
  geom_point(aes(x = x, y = y.noisy), alpha=0.3)+
  geom_line(aes(x=x, y=y))+
  geom_step(aes(x = x, y = fitted.values3), color="darkgrey")+
  theme_light()+
  labs(title="max_depth=4,minsplit=20,minbucket=10", x="",y="")

fancyRpartPlot(tree3, digits=3)