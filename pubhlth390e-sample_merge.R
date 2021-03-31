# Merging your dataset with the confounders

# Group1
gp1 <- read.csv("group1.csv")
conf13 <- read.csv("confounders2013.csv")
gp1comp <- merge(gp1, conf13)
write.csv(gp1comp, "gp1complete.csv")
