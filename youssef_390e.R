library(nhanesA)
library(foreign)
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



demo<- nhanes('DEMO_J')
pfas<- nhanes('PFAS_J')
pfasplus<- nhanes('SSPFAS_J')
liver<- nhanes('LUX_J')
alcohol<- nhanes('ALQ_J')
metals<-nhanes('PBCD_J')
hepa<-nhanes('HEPA_J')
hepc<-nhanes('HEPC_J')
enz<-nhanes('BIOPRO_J')
voc<-nhanes('VOCWB_J')

demo1 <- merge(pfas, liver)
demo2=merge(demo1,demo)
demo3=merge(demo2,metals)
demo4=merge(demo3,hepc)
demo5=merge(demo4,enz)
demo6=merge(demo5,voc)

pfas$pfos=pfas$LBXMFOS+pfas$LBXNFOS
pfas$pfoa=pfas$LBXBFOA+pfas$LBXNFOA

pfas2=pfas[,-c(4,6,8,10,12:20)]
names(pfas2)=c("SEQN","WTSB2YR","pfda","pfhxs","nmefosa","PFNA","pfunda","pfos","pfoa")

base=merge(pfas2,demo5)

# glm.pfos=glm()
