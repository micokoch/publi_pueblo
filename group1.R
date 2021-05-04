library(tidyverse) # If you haven't installed previously, first use: install.packages("tidyverse")

conf2013 = read.csv("confounders2013.csv") # Create object for confounders
group1 = read.csv("group1.csv") # Create object for group 1 variables


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

#Looking more closely at your data
summary(g1all) # Look at summary statistics.
hist(g1all$URXPAR)
quantile(g1all$URXPAR, probs = seq(0, 1, 0.1), na.rm = TRUE)
quantile(g1all$URXPAR, probs = seq(0.9, 1, 0.01), na.rm = TRUE)
largep01 <- dplyr::filter(g1all, g1all$URXPAR > 10)
summary(largep01)

g1all$urxlg2tf <- log2(g1all$URXPAR)
hist(g1all$urxlg2tf)

glm.g1all = glm(dep_sum ~ urxlg2tf, data = g1all)
ggplot(data = glm.g1all, mapping = aes(x = urxlg2tf, y = dep_sum)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")
summary(glm.g1all)
confint(glm.g1all)

lm.g1all = lm(dep_sum ~ urxlg2tf, data = g1all)
ggplot(data = lm.g1all, mapping = aes(x = urxlg2tf, y = dep_sum)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")
summary(lm.g1all)
confint(lm.g1all)

lm.g1allconf = lm(dep_sum ~ urxlg2tf + RIDAGEYR + RIAGENDR + RIDRETH3 + INDHHIN2 + INDFMPIR, data = g1all)
ggplot(data = lm.g1allconf, mapping = aes(x = urxlg2tf, y = dep_sum)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")
summary(lm.g1allconf)
confint(lm.g1allconf)

summary(g1all)
