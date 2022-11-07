library(rpart)
library(rattle)
library(plot3D)
library(plotly)
library(tidyverse)

#dtrain <- as.data.frame(mvrnorm(n = 100, c(9,9),matrix(c(3,0,0,3),2,2)))
#names(dtrain) <- c("x","y")

set.seed(911)

n = 1000

dtrain <- data.frame(x = runif(n,4.5,13.5),y = runif(n,4.5,13.5))

dtrain <- dtrain %>% mutate(z = sqrt((x-9)**2+(y-9)**2))

dtrain %>% ggplot(aes(x = x, y = y)) +
  geom_point()

#Visualización del dataset sintético
plot_ly(dtrain, x = ~x, y = ~y, z = ~z) %>%
  add_markers(size = 1,color = I("red"))

tree <- rpart(z ~ x + y, data = dtrain, method = "anova",maxdepth = 10, minsplit = 1, minbucket = 1, cp = 0)

fitted.values <- predict(tree, newdata = dtrain)

#fancyRpartPlot(tree)

grid.lines = 50   #number of lines on grid
#predict x, y and z variables’ values
x.pred <- seq(4.5, 13.5, length.out = grid.lines)
y.pred <- seq(4.5, 13.5, length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(tree, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

plot_ly(dtrain, x = ~x, y = ~y, z = ~z) %>%
  add_surface(x=x.pred, y=y.pred, z=z.pred,type = 'mesh3d', name = 'pred_surface',
              colorscale = "Greys",opacity = 1) %>%
  add_markers(size = 1) %>% #,color = ~fitted.values
  #list(c(0, 1), c("white", "black"))
  layout(title = 'Datos observados vs. superficie de predicción', plot_bgcolor = "grey")
