library(readr)
library(dplyr)
library(ggplot2)
library(party)
cardio <- read.csv("C:/Users/Omkar n/Downloads/Cardiotocographic.csv")
str(cardio)
cardio$NSPF <- factor(cardio$NSP)
str(cardio)
set.seed(1234)
pd <- sample(2,nrow(cardio),replace = TRUE,prob = c(0.8,0.2))
train <- cardio[pd==1,]
vaildate<- cardio[pd==2,]
# decision tree with party
tree <- ctree(NSPF~LB+AC+FM,data = train)
tree
plot(tree)
 