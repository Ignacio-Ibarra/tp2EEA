library(rpart)
library(tidyverse)
library(gridExtra)

set.seed(911)

n = 100

dtrain <- data.frame(x = runif(n,0,2*pi))

dtrain <- dtrain %>% mutate(y = exp(x/3)*cos(x) + rnorm(n,0,1) )

dtrain %>% ggplot(aes(x = x, y = y)) +
  geom_point()

tree1 <- rpart(y ~ x, data = dtrain, method = "anova",maxdepth = 3, minsplit = 1, minbucket = 1, cp = 0)
fitted.values1 <- predict(tree1, newdata = dtrain)

g1 <- dtrain %>% ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_step(aes(x = x, y = fitted.values1), color="blue")
            
tree2 <- rpart(y ~ x, data = dtrain, method = "anova",maxdepth = 5, minsplit = 1, minbucket = 1, cp = 0)
fitted.values2 <- predict(tree2, newdata = dtrain)

g2 <- dtrain %>% ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_step(aes(x = x, y = fitted.values2), color="red")+theme_light()

tree3 <- rpart(y ~ x, data = dtrain, method = "anova",maxdepth = 4, minsplit = 5, minbucket = 5, cp = 0)
fitted.values3 <- predict(tree3, newdata = dtrain)

g3 <- dtrain %>% ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_step(aes(x = x, y = fitted.values3), color="blue")


grid.arrange(g1, g2, g3, ncol=3)


