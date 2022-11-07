library(rpart)
library(rattle)
library(tidyverse)

#setwd("C://Users//Federico Checozzi//Documents//R//EEA//Trabajo Práctico 2")

dtrain <- read.csv("./datasets/encuesta_salud_train.csv", encoding="UTF-8",stringsAsFactors = TRUE, row.names ="record") 

tree <- rpart(peso ~ altura + edad, data = dtrain, method = "anova",maxdepth = 3, cp = 0)

fitted.values <- predict(tree, newdata = dtrain)

#tree$frame

#tree$splits

fancyRpartPlot(tree)

#gráfico
frame <- tree$frame
nodevec <- as.numeric(row.names(frame[frame$var == "<leaf>",])) #esto genera un vector con los números de nodos terminales
path.list <- path.rpart(tree, nodes = nodevec) #genera una lista en la cual cada elemento indica el camino a un nodo

rect_info <- NULL
for(path in path.list){
  path <- setdiff(path,"root")
  min.h = min(dtrain$altura)
  max.h = max(dtrain$altura)
  min.a = min(dtrain$edad)
  max.a = max(dtrain$edad)
  for(split in path){
    s <- unlist(str_split(split,"< |>="))
    var <- s[1]
    cutoff <- as.numeric(s[2])
    is.less <- str_detect(split,"< ")
    if(var == "altura"){
      if(is.less == TRUE){
        max.h <- cutoff
      } else {
        min.h <- cutoff
      }
    } else {
      if(is.less == TRUE){
        max.a <- cutoff 
      } else {
        min.a <- cutoff
      }
    }
  }
  rect_info <- rbind(rect_info,data.frame(xmin = min.h, xmax = max.h, ymin = min.a, ymax = max.a))
}

ggplot() +
  geom_rect(data = rect_info,aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),colour = "grey50", fill = "white") +
  geom_point(data = dtrain,aes(x = altura, y = edad, color = as.factor(fitted.values)))
