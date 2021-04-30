library(tidyverse) # If you haven't installed previously, first use: install.packages("tidyverse")

conf2015 = read.csv("confounders2015.csv") # Create object for confounders
group3 = read.csv("group3.csv") # Create object for group 3 variables

g3sm = dplyr::select(group3,"SEQN", "URXP01", "CDQ001")
head(g3sm)
summary(g3sm)

g3sm <- dplyr::filter(g3sm, g3sm$CDQ001 < 3) # Remove values greater than 2
summary(g3sm$CDQ001) # Check that all values are between 1 and 2
head(g3sm$CDQ001, n = 15) # Look at first 15 values
# In our data no = 2, but for binary outcome it is typical to have no = 0
g3sm <- g3sm %>% mutate(CDQ001 = replace(CDQ001, CDQ001 == 2, 0)) # Change no to 0
head(g3sm$CDQ001, n = 15) # Now all values for CDQ001 are 0 or 1, and match prior results
hist(g3sm$CDQ001, breaks = 3, xaxp=c(0,1,1))

# Exposure: distribution is skewed
hist(g3sm$URXP01) # Notice that URXCOTT has extreme outliers
quantile(g3sm$URXP01, probs = seq(0, 1, 0.1), na.rm = TRUE) # Carefully look at distribution
g3sm$lg2URXP01 <- log2(g3sm$URXP01) # As in class, let's log2 transform exposure
hist(g3sm$lg2URXP01) # Now we have a bimodal distribution - we'll ignore that for this exercise

g3sm = na.omit(g3sm) # Eliminate observations that have NA values
head(g3sm, n = 15) # Let's take a final look at our dataset

# Merge variables with confounders
g3all = merge(g3sm, conf2015, by="SEQN") # Merge observations that have matching SEQN
head(g3all, n = 15) # Note that some of the confounders have NA, but we won't worry about that
write_csv(g3all, "g3all.csv") # Write a csv file that has all your variables and confounders

# Fit a logistic model to the data without confounders and look at results
glm.g3all = glm(CDQ001 ~ lg2URXP01, family = binomial(link = "logit"), data = g3all)
summary(glm.g3all)
exp(cbind(OR = coef(glm.g3all), confint(glm.g3all)))

# Fit data with confounders and see how data changes
glm.g3allconf = glm(CDQ001 ~ lg2URXP01 + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH3) +
                      INDHHIN2 + INDFMPIR, family = binomial(link = "logit"), data = g3all)
summary(glm.g3allconf)
exp(cbind(OR = coef(glm.g3allconf), confint(glm.g3allconf)))

# Look at a plot of fitted model (even though it's not very informative.)
ggplot(data = glm.g3allconf, mapping = aes(x = lg2URXP01, y = CDQ001)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

