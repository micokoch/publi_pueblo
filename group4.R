library(tidyverse)

conf2015=read.csv("confounders2015.csv")
group4=read.csv("group4.csv")

group4all=merge(group4,conf2015,by="SEQN")
names(group4all)

group4all$dep_sum = rowSums(group4[,c(17:26)])
head(group4all)
group4all=na.omit(group4all)

g4final=select(group4all,"SEQN","URXP01","dep_sum")
head(g4final)

g4all=merge(g4final,conf2015,by="SEQN")
names(g4all)
summary(g4all)
hist(g4all$URXP01)

write_csv(g4all,"g4all.csv")

glm.g4=glm(URXP01~dep_sum,data=g4all)


ggplot(data = glm.g4, mapping = aes(x = URXP01, y = dep_sum)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", color = "red")