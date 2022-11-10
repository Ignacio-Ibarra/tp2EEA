library(rpart)
library(rattle)
library(plot3D)
library(plotly)
library(tidyverse)

setwd("C://Users//Federico Checozzi//Documents//R//EEA//Trabajo Práctico 2")

dtrain <- read.csv("./datasets/encuesta_salud_train.csv", encoding="UTF-8",stringsAsFactors = TRUE, row.names ="record") 

tree <- rpart(peso ~ altura + edad, data = dtrain, method = "anova",maxdepth = 3, cp = 0)

fitted.values <- predict(tree, newdata = dtrain)

fancyRpartPlot(tree)

x <- dtrain$altura
y <- dtrain$edad
z <- dtrain$peso
# predict values on a X-Y grid
grid.lines = 50   #number of lines on grid
#predict x, y and z variables’ values
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( altura = x.pred, edad = y.pred)
z.pred <- matrix(predict(tree, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fit data points to drop lines perpendicular to the grid surface
#fitpoints <- predict(tree)

scatter3D(x,y,z,pch = 18, cex = 1, alpha = 0.7, 
          theta = 220, phi = 20, ticktype = "simple",
          xlab = "altura", ylab = "edad", zlab = "peso",  
          surf = list(x = x.pred, y = y.pred, z = z.pred, col = "black", colvar= NULL, alpha = 1,
                      facets = NA), main = "Datos observados vs. superficie de predicción")

plot_ly(dtrain, x = ~x, y = ~y, z = ~z) %>%
  add_markers(size = 1,color = I("lightblue")) %>% 
  add_surface(x=x.pred, y=y.pred, z=z.pred,type = 'mesh3d', name = 'pred_surface',
              colorscale = "Greys",opacity = 1) %>% 
  #list(c(0, 1), c("white", "black"))
  colorbar(title = 'Peso') %>%
  layout(title = 'Datos observados vs. superficie de predicción', plot_bgcolor = "grey", 
         scene = list(xaxis = list(title = 'Altura'),yaxis = list(title = 'Edad'), zaxis = list(title = 'Peso')))

plot_ly(dtrain, x = ~altura, y = ~edad, z = ~peso) %>%
  add_surface(x=x.pred, y=y.pred, z=z.pred,type = 'mesh3d', name = 'pred_surface',
              colorscale = "Greys",opacity = 1) %>%
  add_markers(size = 1) %>% #,color = ~fitted.values
  #list(c(0, 1), c("white", "black"))
  colorbar(title = 'Peso') %>%
  layout(title = 'Datos observados vs. superficie de predicción', plot_bgcolor = "grey")
