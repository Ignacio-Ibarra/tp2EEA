library(rpart)
library(rattle)
library(tidyverse)

set.seed(911)

n = 1000

dtrain <- data.frame(x = runif(n,4.5,13.5),y = runif(n,4.5,13.5))

dtrain <- dtrain %>% mutate(z = sqrt((x-9)**2+(y-9)**2))

tree <- rpart(z ~ x + y, data = dtrain, method = "anova",maxdepth = 3, minsplit = 1, minbucket = 1, cp = 0)

fancyRpartPlot(tree)

fitted.values <- predict(tree, newdata = dtrain)

frame <- tree$frame
nodevec <- as.numeric(row.names(frame[frame$var == "<leaf>",])) #esto genera un vector con los números de nodos terminales
path.list <- path.rpart(tree, nodes = nodevec) #genera una lista en la cual cada elemento indica el camino a un nodo

rect_info <- NULL
for(path in path.list){
  path <- setdiff(path,"root")
  min.x = min(dtrain$x)
  max.x = max(dtrain$x)
  min.y = min(dtrain$y)
  max.y = max(dtrain$y)
  for(split in path){
    s <- unlist(str_split(split,"< |>="))
    var <- s[1]
    cutoff <- as.numeric(s[2])
    is.less <- str_detect(split,"< ")
    if(var == "x"){
      if(is.less == TRUE){
        max.x <- cutoff
      } else {
        min.x <- cutoff
      }
    } else {
      if(is.less == TRUE){
        max.y <- cutoff 
      } else {
        min.y <- cutoff
      }
    }
  }
  rect_info <- rbind(rect_info,data.frame(xmin = min.x, xmax = max.x, ymin = min.y, ymax = max.y))
}

dtrain <- dtrain %>% 
  mutate(fitted.values = as.factor(round(fitted.values,2)))

label_points <- dtrain %>%
  group_by(fitted.values) %>%
  summarise(x = median(x), y = median (y))

ggplot() +
  geom_rect(data = rect_info,aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),colour = "grey50", fill = "white") +
  geom_point(data = dtrain,aes(x = x, y = y, color = fitted.values)) +
  geom_label(data = label_points,aes(x = x, y = y, label = fitted.values)) +
  labs(color="Valor ajustado")
