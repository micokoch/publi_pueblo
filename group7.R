library(tidyverse) # If you haven't installed previously, first use: install.packages("tidyverse")

conf2017 = read.csv("confounders2017.csv") # Create object for confounders
group7 = read.csv("group7.csv") # Create object for group 3 variables
names(group7)

g7sm = dplyr::select(group7,"SEQN", "LBXCOT", "MCQ010")
head(g7sm)
summary(g7sm)

g7sm <- dplyr::filter(g7sm, g7sm$MCQ010 < 3) # Remove values greater than 2
summary(g7sm$MCQ010) # Check that all values are between 1 and 2
head(g7sm$MCQ010, n = 15) # Look at first 15 values
# In our data no = 2, but for binary outcome it is typical to have no = 0
g7sm <- g7sm %>% mutate(MCQ010 = replace(MCQ010, MCQ010 == 2, 0)) # Change no to 0
head(g7sm$MCQ010, n = 15) # Now all values for MCQ010 are 0 or 1, and match prior results
hist(g7sm$MCQ010, breaks = 3, xaxp=c(0,1,1))

# Exposure: distribution is skewed
hist(g7sm$LBXCOT) # Notice that URXCOTT has extreme outliers
quantile(g7sm$LBXCOT, probs = seq(0, 1, 0.1), na.rm = TRUE) # Carefully look at distribution
g7sm$lg2LBXCOT <- log2(g7sm$LBXCOT) # As in class, let's log2 transform exposure
hist(g7sm$lg2LBXCOT) # Now we have a bimodal distribution - we'll ignore that for this exercise

g7sm = na.omit(g7sm) # Eliminate observations that have NA values
head(g7sm, n = 15) # Let's take a final look at our dataset

# Merge variables with confounders
g7all = merge(g7sm, conf2017, by="SEQN") # Merge observations that have matching SEQN
head(g7all, n = 15) # Note that some of the confounders have NA, but we won't worry about that
write_csv(g7all, "g7all.csv") # Write a csv file that has all your variables and confounders

summary(g7all)

# Fit a logistic model to the data without confounders and look at results
glm.g7all = glm(MCQ010 ~ lg2LBXCOT, family = binomial(link = "logit"), data = g7all)
summary(glm.g7all)
exp(cbind(OR = coef(glm.g7all), confint(glm.g7all)))

# Fit data with confounders and see how data changes
glm.g7allconf = glm(MCQ010 ~ lg2LBXCOT + RIDAGEYR + factor(RIAGENDR) + factor(RIDRETH3) +
                      INDHHIN2 + INDFMPIR, family = binomial(link = "logit"), data = g7all)
summary(glm.g7allconf)
exp(cbind(OR = coef(glm.g7allconf), confint(glm.g7allconf)))

# Look at a plot of fitted model (even though it's not very informative.)
ggplot(data = glm.g7allconf, mapping = aes(x = lg2LBXCOT, y = MCQ010)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

