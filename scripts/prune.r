library(tidymodels)
library(rpart)
library(rattle)
library(tidyverse)

setwd("C://Users//Federico Checozzi//Documents//R//EEA//Trabajo Práctico 2")

dtrain <- read.csv("./datasets/encuesta_salud_train.csv", encoding="UTF-8",stringsAsFactors = TRUE, row.names ="record") 

base_tree <- rpart(peso ~ ., data = dtrain, method = "anova",maxdepth = 6, minsplit = 1, minbucket = 1, cp = 0)

printcp(base_tree)

rsq.rpart(base_tree)

cptable <- as.data.frame(base_tree$cptable)

cptablemin <- cptable[cptable$xerror == min(cptable$xerror),]

cptablemin

cptablemin$xerror + cptablemin$xstd

cptable[1:(cptablemin$nsplit+1),] 

cpopt <- cptable[7,"CP"] #elegido por regla 1-SE

pruned_tree <- prune(base_tree, cp = cpopt)

#alternativamente podría haber usado
pruned_tree <- rpart(peso ~ ., data = dtrain, method = "anova",maxdepth = 6, minsplit = 1, minbucket = 1, cp = cpopt)

