library(tidyverse) # If you haven't installed previously, first use: install.packages("tidyverse")

conf2015 = read.csv("confounders2015.csv") # Create object for confounders
group2 = read.csv("group2.csv") # Create object for group 2 variables

names(group2) # Check variable names

# Create a new object that only has the variables of interest (confounders haven't been added yet)
g2sm = dplyr::select(group2,"SEQN", "URXCOTT", "KIQ022") # Limit variables to exposure & outcome

# Looking more closely at your data
summary(g2sm) # Look at summary statistics - problems with outcome and exposure
# Outcome - should be binary with only 2 values
# Exposure - distribution is skewed due to outliers (compare median, mean, quartiles)

# Outcome: in the codebook, we see that for KIQ022, 7 means refused and 9 means don't know
g2sm <- dplyr::filter(g2sm, g2sm$KIQ022 < 3) # Remove values greater than 2
summary(g2sm$KIQ022) # Check that all values are between 1 and 2
head(g2sm$KIQ022, n = 15) # Look at first 10 values
# In our data no = 2, but for binary outcome it is typical to have no = 0
g2sm <- g2sm %>% mutate(KIQ022 = replace(KIQ022, KIQ022 == 2, 0)) # Change no to 0
head(g2sm$KIQ022, n = 15) # Now all values for KIQ022 are 0 or 1, and match prior results
hist(g2sm$KIQ022, breaks = 3, xaxp=c(0,1,1))

# Exposure: distribution is skewed
hist(g2sm$URXCOTT) # Notice that URXCOTT has extreme outliers
quantile(g2sm$URXCOTT, probs = seq(0, 1, 0.1), na.rm = TRUE) # Carefully look at distribution
g2sm$lg2URXCOTT <- log2(g2sm$URXCOTT) # As in class, let's log2 transform exposure
hist(g2sm$lg2URXCOTT) # Now we have a bimodal distribution - we'll ignore that for this exercise

g2sm = na.omit(g2sm) # Eliminate observations that have NA values
head(g2sm, n = 15) # Let's take a final look at our dataset

# Merge variables with confounders
g2all = merge(g2sm, conf2015, by="SEQN") # Merge observations that have matching SEQN
head(g2all, n = 15) # Note that some of the confounders have NA, but we won't worry about that
write_csv(g2all, "g2all.csv") # Write a csv file that has all your variables and confounders

# Fit a logistic model to the data without confounders and look at results
glm.g2all = glm(KIQ022 ~ lg2URXCOTT, family = binomial(link = "logit"), data = g2all)
summary(glm.g2all)
exp(cbind(OR = coef(glm.g2all), confint(glm.g2all)))

# Fit data with confounders and see how data changes
glm.g2allconf = glm(KIQ022 ~ lg2URXCOTT + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH3) +
                      INDHHIN2 + INDFMPIR, family = binomial(link = "logit"), data = g2all)
summary(glm.g2allconf)
exp(cbind(OR = coef(glm.g2allconf), confint(glm.g2allconf)))

# Look at a plot of fitted model (even though it's not very informative.)
ggplot(data = glm.g2allconf, mapping = aes(x = lg2URXCOTT, y = KIQ022)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "lm", color = "blue")

