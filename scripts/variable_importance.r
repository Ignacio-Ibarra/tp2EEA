library(rpart)
library(tidyverse)

#setwd("C://Users//Federico Checozzi//Documents//R//EEA//Trabajo Práctico 2")
setwd("C://Users//tiama//OneDrive//Documentos//R//EEA//Trabajo Práctico 2")

dtrain <- read.csv("./datasets/encuesta_salud_train.csv", encoding="UTF-8",stringsAsFactors = TRUE, row.names ="record") 

tree <- rpart(peso ~ ., data = dtrain, method = "anova")

#fitted.values <- predict(tree, newdata = dtrain)

summary(tree) #incluye importancia relativa en aquellos casos que el porcentaje sea mayor a 1%

#fuente: https://stackoverflow.com/questions/56304698/how-do-i-plot-the-variable-importance-of-my-trained-rpart-decision-tree-model

#importancia absoluta

df <- data.frame(imp = tree$variable.importance)
df2 <- df %>% 
  rownames_to_column() %>% 
  rename("variable" = rowname) %>% 
  arrange(imp) %>%
  mutate(variable = forcats::fct_inorder(variable))
ggplot(df2) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()

#importancia relativa

df3 <- df2 %>% 
  mutate(imp = imp/sum(imp) * 100)
ggplot(df3) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw() + ylab("Importancia de variables [%]") +xlab("")
