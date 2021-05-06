library(tidyverse)

conf2013 = read.csv("confounders2013.csv") # Create object for confounders
group6 = read.csv("group6.csv") # Create object for group 6 variables

# Create a new object that only has the variables of interest (confounders haven't been added yet)
names(group6)
g6sm = dplyr::select(group6,"SEQN", "URXP01", "CFDAST") # Limit your variables to those used for exposure and outcome
head(g6sm, n = 20) # Look at the first 20 rows of your object
g6sm = na.omit(g6sm) # Eliminate observations that have NA values
head(g6sm, n = 20) # Check the first 20 rows again

# Merge your dataset with the confounders dataset
g6all = merge(g6sm, conf2013, by="SEQN") # This will only merge the observations that have matching SEQN
head(g6all, n = 20) # Look at first 20 observations - note that some of the confounders have NA, but we won't worry about that
write_csv(g6all, "g6all.csv") # Write a csv file that has all your variables and confounders

#Looking more closely at your data
summary(g6all) # Look at summary statistics.
hist(g6all$URXP01)
quantile(g6all$URXP01, probs = seq(0, 1, 0.1), na.rm = TRUE)
quantile(g6all$URXP01, probs = seq(0.9, 1, 0.01), na.rm = TRUE)

g6all$urxp1lg2tf <- log2(g6all$URXP01)
hist(g6all$urxp1lg2tf)

glm.g6all = glm(CFDAST ~ urxp1lg2tf, data = g6all)
ggplot(data = glm.g6all, mapping = aes(x = urxp1lg2tf, y = CFDAST)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "lm", color = "blue")

glm.g6allconf = glm(CFDAST ~ urxp1lg2tf + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH3) + INDHHIN2 + INDFMPIR, data = g6all)
ggplot(data = glm.g6allconf, mapping = aes(x = urxp1lg2tf, y = CFDAST)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "lm", color = "blue")
summary(glm.g6allconf)

lm.g6allconf = lm(CFDAST ~ urxp1lg2tf + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH3) + INDHHIN2 + INDFMPIR, data = g6all)
ggplot(data = lm.g6allconf, mapping = aes(x = urxp1lg2tf, y = CFDAST)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")
summary(lm.g6allconf)
confint(lm.g6allconf)

