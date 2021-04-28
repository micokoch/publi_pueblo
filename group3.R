library(tidyverse) # If you haven't installed previously, first use: install.packages("tidyverse")

conf2015 = read.csv("confounders2015.csv") # Create object for confounders
group3 = read.csv("group3.csv") # Create object for group 3 variables

g3sm = dplyr::select(group3,"SEQN", "URXP01", "CDQ001")
head(g3sm)

g3all = merge(g3sm, conf2015, by="SEQN")
head(g3all)

write_csv(g3all, "g3all.csv")


