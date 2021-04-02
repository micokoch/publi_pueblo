# PubHlth 390e Script for datasets
library(tidyverse)
library(nhanesA)

#Confounders 2013
demogconf2013 <- nhanes('DEMO_H') %>% 
  select(c('SEQN', 'RIDAGEYR', 'RIAGENDR', 'RIDRETH3', 'INDHHIN2', 'INDFMPIR'))
#head(demogconf2013)
write_csv(demogconf2013, "confounders2013.csv")

#Confounders 2015
demogconf2015 <- nhanes('DEMO_I') %>% 
  select(c('SEQN', 'RIDAGEYR', 'RIAGENDR', 'RIDRETH3', 'INDHHIN2', 'INDFMPIR'))
#head(demogconf2015)
write_csv(demogconf2015, "confounders2015.csv")

#Confounders 2017
demogconf2017 <- nhanes('DEMO_J') %>% 
  select(c('SEQN', 'RIDAGEYR', 'RIAGENDR', 'RIDRETH3', 'INDHHIN2', 'INDFMPIR'))
#head(demogconf2017)
write_csv(demogconf2017, "confounders2017.csv")

# Group 1
orgphos2013 <- nhanes('UPHOPM_H')
depress2013 <- nhanes('DPQ_H')
group1 <- merge(orgphos2013, depress2013, by = "SEQN")
write_csv(group1, "group1.csv")
# Group 1 and Confounders
g1_comp = merge(group1, demogconf2013, by = "SEQN")
write_csv(g1_comp, "g1_complete.csv")

# Group 2
nicoturi2015 <- nhanes('UCOT_I')
kidney2015 <- nhanes('KIQ_U_I')
group2 <- merge(nicoturi2015, kidney2015, by = "SEQN")
write_csv(group2, "group2.csv")
# Group 2 and Confounders
g2_comp = merge(group2, demogconf2015, by = "SEQN")
write_csv(g2_comp, "g2_complete.csv")

# Group 3
pah2015 <- nhanes('PAH_I')
heart2015 <- nhanes('CDQ_I')
group3 <- merge(pah2015, heart2015, by = "SEQN")
write_csv(group3, "group3.csv")
# Group 3 and Confounders
g3_comp = merge(group3, demogconf2015, by = "SEQN")
write_csv(g3_comp, "g3_complete.csv")

# Group 4
pah2015 <- nhanes('PAH_I')
depress2015 <- nhanes('DPQ_I')
group4 <- merge(pah2015, heart2015, by = "SEQN")
write_csv(group4, "group4.csv")
# Group 4 and Confounders
g4_comp = merge(group4, demogconf2015, by = "SEQN")
write_csv(g4_comp, "g4_complete.csv")

# Group 5
bpa2015 <-nhanes('EPHPP_I')
body2015 <- nhanes('BMX_I')
group5 <- merge(bpa2015, body2015, by = "SEQN")
write_csv(group5, "group5.csv")
# Group 5 and Confounders
g5_comp = merge(group5, demogconf2015, by = "SEQN")
write_csv(g5_comp, "g5_complete.csv")

# Group 6
pah2013 <- nhanes('PAH_H')
cog2013 <- nhanes('CFQ_H')
group6 <- merge(pah2013, cog2013, by = "SEQN")
write_csv(group6, "group6.csv")
# Group 6 and Confounders
g6_comp = merge(group6, demogconf2013, by = "SEQN")
write_csv(g6_comp, "g6_complete.csv")

# Group 7
smoking2017 <- nhanes('SMQ_J')
cotininebl2017 <- nhanes('COT_J')
leadbl2017 <- nhanes('PBCD_J')
group7 <- merge(smoking2017, cotininebl2017, by = "SEQN") %>% 
  merge(leadbl2017, by = "SEQN")
write_csv(group7, "group7.csv")
# Group 7 and Confounders
g7_comp = merge(group7, demogconf2017, by = "SEQN")
write_csv(g7_comp, "g7_complete.csv")

# Youssef
demo <- nhanes('DEMO_J')
pfas <- nhanes('PFAS_J')
pfasplus <- nhanes('SSPFAS_J')
liver <- nhanes('LUX_J')
alcohol <- nhanes('ALQ_J')
metals <- nhanes('PBCD_J')
hepa <- nhanes('HEPA_J')
hepc <- nhanes('HEPC_J')
enz <- nhanes('BIOPRO_J')
voc <- nhanes('VOCWB_J')

demo1 = merge(pfas, liver, by = "SEQN")
demo2 = merge(demo1,demo, by = "SEQN")
demo3 = merge(demo2,metals, by = "SEQN")
demo4 = merge(demo3,hepc, by = "SEQN")
demo5 = merge(demo4,enz, by = "SEQN")
demo6 = merge(demo5,voc, by = "SEQN")

#ademo1 = merge(pfas, liver, all = TRUE)
#ademo2 = merge(ademo1,demo, all = TRUE)
#ademo3 = merge(ademo2,metals, all = TRUE)
#ademo4 = merge(ademo3,hepc, all = TRUE)
#ademo5 = merge(ademo4,enz, all = TRUE)
#ademo6 = merge(ademo5,voc, all = TRUE)

pfas$pfos = pfas$LBXMFOS+pfas$LBXNFOS
pfas$pfoa = pfas$LBXBFOA+pfas$LBXNFOA

pfas2 = pfas[,-c(4,6,8,10,12:20)]
names(pfas2) = c("SEQN","WTSB2YR","pfda","pfhxs","nmefosa","PFNA","pfunda","pfos","pfoa")

base = merge(pfas2,demo5, by = "SEQN")
#base

#glm.pfos = glm(pfos ~ INDFMPIR, data = base, family = Gamma)
#summary(glm.pfos) %>% pander()

#ggplot(data = glm.pfos, mapping = aes(x = pfos, y = INDFMPIR)) +
#  geom_point(size = 0.5, alpha = 0.5) +
#  geom_smooth(method = "loess", color = "red")

#abase = merge(pfas2, ademo5, all = TRUE)
#abase

#a.glm.pfos = glm(pfos ~ INDFMPIR, data = abase, family = gaussian)
#summary(a.glm.pfos) %>% pander()

#ggplot(data = a.glm.pfos, mapping = aes(x = pfos, y = INDFMPIR)) +
#  geom_point(size = 0.5, alpha = 0.5) +
#  geom_smooth(method = "loess", color = "blue")

write_csv(base, "youssef.csv")
#write_csv(abase, "abase.csv")

