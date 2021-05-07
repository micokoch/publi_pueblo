library(tidyverse)

conf2015 = read.csv("confounders2015.csv") # Create object for confounders
group5 = read.csv("group5.csv") # Create object for group 6 variables

# Create a new object that only has the variables of interest (confounders haven't been added yet)
names(group5)
g5sm = dplyr::select(group5,"SEQN", "URXBPH", "BMXBMI") # Limit your variables to those used for exposure and outcome
head(g5sm, n = 20) # Look at the first 20 rows of your object
g5sm = na.omit(g5sm) # Eliminate observations that have NA values
head(g5sm, n = 20) # Check the first 20 rows again

# Merge your dataset with the confounders dataset
g5all = merge(g5sm, conf2015, by="SEQN") # This will only merge the observations that have matching SEQN
head(g5all, n = 20) # Look at first 20 observations - note that some of the confounders have NA, but we won't worry about that
write_csv(g5all, "g5all.csv") # Write a csv file that has all your variables and confounders

#Looking more closely at your data
summary(g5all) # Look at summary statistics.
hist(g5all$URXBPH)
quantile(g5all$URXBPH, probs = seq(0, 1, 0.1), na.rm = TRUE)
quantile(g5all$URXBPH, probs = seq(0.9, 1, 0.01), na.rm = TRUE)

g5all$urxbphlg2tf <- log2(g5all$URXBPH)
hist(g5all$urxbphlg2tf)

glm.g5all = glm(BMXBMI ~ urxbphlg2tf, data = g5all)
ggplot(data = glm.g5all, mapping = aes(x = urxbphlg2tf, y = BMXBMI)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "lm", color = "blue")

glm.g5allconf = glm(BMXBMI ~ urxbphlg2tf + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH3) + INDHHIN2 + INDFMPIR, data = g5all)
ggplot(data = glm.g5allconf, mapping = aes(x = urxbphlg2tf, y = BMXBMI)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "lm", color = "blue")
summary(glm.g5allconf)

lm.g5allconf = lm(BMXBMI ~ urxbphlg2tf + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH3) + INDHHIN2 + INDFMPIR, data = g5all)
ggplot(data = lm.g5allconf, mapping = aes(x = urxbphlg2tf, y = BMXBMI)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")
summary(lm.g5allconf)
confint(lm.g5allconf)

