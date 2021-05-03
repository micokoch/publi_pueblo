library(tidyverse) # If you haven't installed previously, first use: install.packages("tidyverse")

conf2015 = read.csv("confounders2015.csv") # Create object for confounders
group4 = read.csv("group4.csv") # Create object for group 4 variables


# Create a new variable "dep_sum" that is the sum of DPQ010 to DPQ100

g4fix <- group4
g4fix <- g4fix %>% 
  dplyr::filter(DPQ010 < 3) %>% 
  dplyr::filter(DPQ020 < 3) %>% 
  dplyr::filter(DPQ030 < 3) %>% 
  dplyr::filter(DPQ040 < 3) %>% 
  dplyr::filter(DPQ050 < 3) %>% 
  dplyr::filter(DPQ060 < 3) %>% 
  dplyr::filter(DPQ070 < 3) %>% 
  dplyr::filter(DPQ080 < 3) %>% 
  dplyr::filter(DPQ090 < 3) %>% 
  dplyr::filter(DPQ100 < 3)

names(g4fix) # Check what the column numbers are for DPQ010 to DPQ100 - 17 through 26
g4fix$dep_sum = rowSums(g4fix[, c(17:26)]) # Create dep_sum variable
head(g4fix, n = 10) # Check that deep_sum is the sum of DPQ010 to DPQ100 by looking at 10 first rows
hist(g4fix$dep_sum)

# Create a new object that only has the variables of interest (confounders haven't been added yet)
g4sm = dplyr::select(g4fix,"SEQN", "URXP01", "dep_sum") # Limit your variables to those used for exposure and outcome
head(g4sm, n = 20) # Look at the first 20 rows of your object
g4sm = na.omit(g4sm) # Eliminate observations that have NA values
head(g4sm, n = 20) # Check the first 20 rows again

# Merge your dataset with the confounders dataset
g4all = merge(g4sm, conf2015, by="SEQN") # This will only merge the observations that have matching SEQN
head(g4all, n = 20) # Look at first 20 observations - note that some of the confounders have NA, but we won't worry about that
write_csv(g4all, "g4all.csv") # Write a csv file that has all your variables and confounders

#Looking more closely at your data
summary(g4all) # Look at summary statistics.
# As we saw during office hours, URXP01 has some extreme outliers. You can see that the median is 1 495 but the mean is 44 276
# This suggests there are extreme outliers - even the 3rd quartile is far below the the mean (5 427).
# We will look at this variable more carefully.

hist(g4all$URXP01) # First we can look at a histogram which is not very informative
quantile(g4all$URXP01, probs = seq(0, 1, 0.1), na.rm = TRUE) # We can observe that the distribution is fairly normal (up to 90th %)
# Let's look at the distribution between the 90 -100th percentile more closely
quantile(g4all$URXP01, probs = seq(0.9, 1, 0.01), na.rm = TRUE)
# Even up to the 97th percentile the growth is gradual, and then it skyrockets after the 99th percentile

largep01 <- dplyr::filter(g4all, g4all$URXP01 > 50000) # Create an object with only these large observations
summary(largep01) # There are only 32 observations > 50 000 (out of 1 100), so in this first analysis we'll remove them.
# By doing this, we're still including > 97% of all observations (as we saw previously with quantiles).

# Remove outliers where URXP01 > 50 000
g4rmout <- dplyr::filter(g4all, g4all$URXP01 < 50000)
summary(g4rmout)
hist(g4rmout$URXP01) # First, let's see how the variable data are now distributed
# The data are still very skewed to the right - we can log transform the data to correct it
g4rmout$urxlntf <- log(g4rmout$URXP01)
summary(g4rmout)
hist(g4rmout$urxlntf) # Now the data look normally distributed

# Just in case, let's see what the distribution looks like without removing the extreme outliers
g4all$urxlntf <- log(g4all$URXP01)
hist(g4all$urxlntf) # The distribution looks less normal, and so we'll analyze the data with extreme outliers removed

# Let's do a linear regression with just the exposure and outcome
glm.g4 = glm(dep_sum ~ urxlntf, data = g4rmout)

# Now let's plot it
ggplot(data = glm.g4, mapping = aes(x = urxlntf, y = dep_sum)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

# It doesn't look great, and there isn't an obvious pattern. We can try and add confounders to see if it makes a difference.
glm.g4conf = glm(dep_sum ~ urxlntf + RIDAGEYR + RIAGENDR + RIDRETH3 + INDHHIN2 + INDFMPIR, data = g4rmout)

ggplot(data = glm.g4conf, mapping = aes(x = urxlntf, y = dep_sum)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

# Not really much better, but we can talk about this data in class. "dep_sum" can be made into a bivariate outcome perhaps.
# To start with, we can look at the full dataset without removing outliers

# Let's do a linear regression with just the exposure and outcome for the full dataset (with outliers)
glm.g4all = glm(dep_sum ~ urxlntf, data = g4all)

# Now let's plot it
ggplot(data = glm.g4all, mapping = aes(x = urxlntf, y = dep_sum)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

# It looks better for extreme values (although there are few data points). Now try with all confounders.
glm.g4allconf = glm(dep_sum ~ urxlntf + RIDAGEYR + RIAGENDR + RIDRETH3 + INDHHIN2 + INDFMPIR, data = g4all)

ggplot(data = glm.g4allconf, mapping = aes(x = urxlntf, y = dep_sum)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

# Hope this helps!

#############
# Improved analysis
g4all$urxlog2tf <- log2(g4all$URXP01)
hist(g4all$urxlog2tf)

glm.g4allfinal = glm(dep_sum ~ urxlog2tf + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH3) + INDHHIN2 + INDFMPIR, data = g4all)
summary(glm.g4allfinal)
confint(glm.g4allfinal)

ggplot(data = glm.g4allfinal, mapping = aes(x = urxlog2tf, y = dep_sum)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

# Linear model
lm.g4all = lm(dep_sum ~ urxlntf, data = g4all)
summary(lm.g4all)
confint(lm.g4all)

# Now let's plot it
ggplot(data = lm.g4all, mapping = aes(x = urxlntf, y = dep_sum)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "lm", color = "blue")


#### Youssef functions
library(summarytools)

# view(dfSummary(base2))
view(dfSummary(g4all))

# plot(LUXCAPM ~ LBXNFOA, data = base2)
plot(dep_sum ~ URXP01, data = g4all)

# plot(LUXCAPM ~ log(LBXNFOA), data = base2)
plot(dep_sum ~ urxlntf, data = g4all)


# lm.pfoa.univar=lm(LUXCAPM ~ LBXNFOA,data=base2)
# final.data=subset(data5,KIQ022!=9)

# lm.pfoa.univar=lm(LUXCAPM ~ LBXNFOA,data=base2)

# summary(lm.pfoa.univar)

# summary(lm.pfoa.univar)

# lm.pfoa=lm(LUXCAPM  ~ log2(LBXNFOA)+factor(RIAGENDR)+RIDAGEYR+factor(RIDRETH3)+INDFMPIR,data=base2)
# summary(lm.pfoa)

