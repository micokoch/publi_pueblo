library(tidyverse)
library(nhanesA)
library(foreign)
library(pander)
nhanesTables('EXAM', 2017)


q2017names  <- nhanesTables('Q', 2017, namesonly=TRUE)
q2017tables <- lapply(q2017names, nhanes)
names(q2017tables) <- q2017names
nhanesTables('Q', 2017)

d2017names  <- nhanesTables('DIET', 2017, namesonly=TRUE)
d2017tables <- lapply(d2017names, nhanes)
names(d2017tables) <- d2017names
nhanesTables('DIET', 2017)

demo2017names  <- nhanesTables('DEMO', 2017, namesonly=TRUE)
demo2017tables <- lapply(demo2017names, nhanes)
names(demo2017tables) <- demo2017names
nhanesTables('DEMO', 2017)

exam2017names  <- nhanesTables('EXAM', 2017, namesonly=TRUE)
exam2017tables <- lapply(exam2017names, nhanes)
names(exam2017tables) <- exam2017names
nhanesTables('EXAM', 2017)

lab2017names  <- nhanesTables('LAB', 2017, namesonly=TRUE)
lab2017tables <- lapply(lab2017names, nhanes)
names(lab2017tables) <- lab2017names
nhanesTables('LAB', 2017)



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

demo1 = merge(pfas, liver)
demo2 = merge(demo1,demo)
demo3 = merge(demo2,metals)
demo4 = merge(demo3,hepc)
demo5 = merge(demo4,enz)
demo6 = merge(demo5,voc)

ademo1 = merge(pfas, liver, all = TRUE)
ademo2 = merge(ademo1,demo, all = TRUE)
ademo3 = merge(ademo2,metals, all = TRUE)
ademo4 = merge(ademo3,hepc, all = TRUE)
ademo5 = merge(ademo4,enz, all = TRUE)
ademo6 = merge(ademo5,voc, all = TRUE)

pfas$pfos = pfas$LBXMFOS+pfas$LBXNFOS
pfas$pfoa = pfas$LBXBFOA+pfas$LBXNFOA

pfas2 = pfas[,-c(4,6,8,10,12:20)]
names(pfas2) = c("SEQN","WTSB2YR","pfda","pfhxs","nmefosa","PFNA","pfunda","pfos","pfoa")

base = merge(pfas2,demo5)
base

glm.pfos = glm(pfos ~ INDFMPIR, data = base, family = Gamma)
summary(glm.pfos) %>% pander()

ggplot(data = glm.pfos, mapping = aes(x = pfos, y = INDFMPIR)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", color = "red")

abase = merge(pfas2, ademo5, all = TRUE)
abase

a.glm.pfos = glm(pfos ~ INDFMPIR, data = abase, family = gaussian)
summary(a.glm.pfos) %>% pander()

ggplot(data = a.glm.pfos, mapping = aes(x = pfos, y = INDFMPIR)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue")

write_csv(base, "base.csv")
write_csv(abase, "abase.csv")


# Group 1
orgphos2013 <- nhanes('UPHOPM_H')
depress2013 <- nhanes('DPQ_H')
group1 <- merge(orgphos2013, depress2013)
write_csv(group1, "group1.csv")

# Group 2
nicoturi2014 <- nhanes('UCOT_I')
kidney014 <- nhanes('KIQ_U_I')
group2 <- merge(nicoturi2014, kidney014)
write_csv(group2, "group2.csv")
