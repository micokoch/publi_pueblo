library(tidyverse)

conf2013 = read.csv("confounders2013.csv")
group1 = read.csv("group1.csv")

# Create a new variable "dep_sum" that is the sum of DPQ010 to DPQ100
names(group1) # Check what the column numbers are for DPQ010 to DPQ100 - 17 through 26
group1$dep_sum = rowSums(group1[, c(17:26)]) # Create dep_sum variable
head(group1, n = 10) # Check that deep_sum is the sum of DPQ010 to DPQ100 by looking at 10 first rows

# Create a new object that only has the variables of interest (confounders haven't been added yet)
g1sm = dplyr::select(group1,"SEQN", "URXPAR", "dep_sum") # Limit your variables to those used for exposure and outcome
head(g1sm, n = 20) # Look at the first 20 rows of your object
g1sm = na.omit(g1sm) # Eliminate observations that have NA values
head(g1sm, n = 20) # Check the first 20 rows again

# Merge your dataset with the confounders dataset
g1all = merge(g1sm, conf2013, by="SEQN") # This will only merge the observations that have matching SEQN
head(g1all, n = 20) # Look at first 20 observations - note that some of the confounders have NA, but we won't worry about that
write_csv(g1all, "g1all.csv") # Write a csv file that has all your variables and confounders

summary(g1all)
hist(g1all$URXPAR)
quantile(g1all$URXPAR, probs = seq(0, 1, 0.1), na.rm = TRUE)
quantile(g1all$URXPAR, probs = seq(0.9, 1, 0.01), na.rm = TRUE)
largep01 <- dplyr::filter(g1all, g1all$URXPAR > 10)
summary(largep01)

g1all$urxlntf <- log(g1all$URXPAR)
hist(g1all$urxlntf)

glm.g1all = glm(dep_sum ~ urxlntf, data = g1all)
ggplot(data = glm.g1all, mapping = aes(x = urxlntf, y = dep_sum)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "lm", color = "blue")

glm.g1allconf = glm(dep_sum ~ urxlntf + RIDAGEYR + RIAGENDR + RIDRETH3 + INDHHIN2 + INDFMPIR, data = g1all)
ggplot(data = glm.g1allconf, mapping = aes(x = urxlntf, y = dep_sum)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "lm", color = "blue")

#####
g1med = dplyr::select(group1, SEQN, URXPAR, DPQ010:dep_sum)
head(g1med, n=20)
g1med = na.omit(g1med)
head(g1med, n = 20)
g1final = merge(g1med, conf2013, by="SEQN")
g1final$urxparlog2tf <- log2(g1all$URXPAR)
hist(g1final$urxparlog2tf)
head(g1final, n = 20)
write_csv(g1final, "g1final.csv")
