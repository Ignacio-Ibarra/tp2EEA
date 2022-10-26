rm( list=ls() )  
gc()  

require(data.table)
require(ggplot2)
require(rpart)
require(rpart.plot)
require(gridExtra)
require(parttree)

df <- fread("./datasets/encuesta_salud_train.csv")

# obtengo una muestra
N = 100
set.seed(123)
idx <- sample(1:nrow(df), N)
samp.df <- df[idx, ]

min.x1 <- min(samp.df$altura)
max.x1 <- max(samp.df$altura)

# Modelo Lineal Simple: Peso  ~ Altura
lm.simple <- lm("peso ~ altura", data=samp.df)
coefs <- lm.simple$coefficients
g1 <- ggplot(samp.df, aes(altura, peso))+
  geom_point()+
  geom_abline(intercept = coefs[1], slope = coefs[2])

arbol <- rpart(formula = "peso ~ altura",
               data = samp.df,
               method = "anova",
               )
min.x1 <- min(samp.df$altura)
max.x1 <- max(samp.df$altura)

x1 <- c(min.x1,161,161,165,165,168,168,173,173,179,179)
x2 <- c(161,161,165,165,168,168,173,173,179,179,max.x1)
y1 <- c(52,52,64,64,53,53,68,68,63,63,75)
y2 <- c(52,64,64,53,53,68,68,63,63,75,75)

partitions <- data.frame(x1, x2, y1, y2)

g2 <- ggplot(samp.df, aes(altura, peso))+
  geom_point()+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = partitions)
  


grid.arrange(g1,g2)


