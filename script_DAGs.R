
library(ggdag)
library(ggplot2)
#Better DAG
xx<-dagify(SRS ~ Phthalates + MSmoking+MAge+Race+MStatus+FA+MAlcohol+Sex+Parity+Site+Year,
           Phthalates ~ Year+MEduc+Income+Parity+Site+MStatus+MAge,
           MSmoking ~ MEduc+Income+Country,
           MEduc ~ Country+Race,
           Income ~ Country+MStatus+Race,
           MStatus ~ MAge+Country+Race,
           MAge ~ Country+Race+MEduc,
           FA ~ MEduc,
           MSmoking~Country+Race+MEduc+Income,
           MAlcohol~Country+Race+MEduc+Income,
           Parity ~ Income+MAge+Race+MEduc,
           MEduc ~~ Income, # bidirected path
           MAlcohol ~~ MSmoking, # bidirected path
           labels=c("Phthalates"="Phthalates","MSmoking"="Maternal\n Smoking", "MAlcohol"="Maternal\n Alcohol", "MAge"="Maternal\n Age",
                    "Race"="Race\n Ethnicity","MStatus"="Marital\n Status","FA"="Folic\n Acid", "Sex"="Sex", "Year"="Year of\n Enrollment",
                    "Parity"="Parity","Site"="Study\n Site","Country"="Mother's country\n of birth","Income"="Household\n Income",
                    "MEduc"="Maternal\n Education","SRS"="Autistic\n Traits"),
           exposure = "Phthalates",
           outcome = "SRS")

ggdag(xx, text = FALSE, use_labels = "label")

ggdag_paths(xx, text = FALSE, use_labels = "label", shadow = TRUE)
ggdag_adjustment_set(xx, text = FALSE, use_labels = "label", shadow = TRUE)


#Latent DAG
xx<-dagify(SRS ~ Phthalates + MSmoking+MAge+SES+FA+MAlcohol+Sex+Parity+Site+Year+MDepression+MStress+Breastfeeding+BLL,
           Phthalates ~ Year+SES+Parity+Site+MAge+FA,
           MSmoking ~ SES,
           SES ~ MEduc+Income+Race+MStatus,
           Income ~ MEduc+Country+MStatus+Race,
           MStatus ~ MAge+Country+Race,
           MEduc~ Country+Income+Race+MStatus,
           MAge ~ Country+Race+MEduc,
           FA ~ MEduc,
           MSmoking~SES,
           MAlcohol~SES,
           Parity ~ MAge+SES,
           MDepression ~ SES+Parity+Phthalates+MAge,
           MStress ~ SES+Parity+MAge,
           Breastfeeding ~ SES+Phthalates+MSmoking+MAlcohol+Parity+MAge,
           BLL~SES+MSmoking+MAge,
           MAlcohol ~~ MSmoking, # bidirected path
           labels=c("Breastfeeding"="Breastfeeding","Phthalates"="Phthalates","MSmoking"="Maternal\n Smoking", "MAlcohol"="Maternal\n Alcohol", "MAge"="Maternal\n Age",
                    "Race"="Race\n Ethnicity","MStatus"="Marital\n Status","FA"="Folic\n Acid", "Sex"="Sex", "Year"="Year of\n Enrollment",
                    "Parity"="Parity","Site"="Study\n Site","Country"="Mother's country\n of birth","Income"="Household\n Income",
                    "MEduc"="Maternal\n Education","SRS"="Autistic\n Traits","SES"="Socio-economic\n Status","MDepression"="Maternal\n Depression\n 3yrs",
                    "MStress"="Maternal\n Stress\n 3yrs","BLL"="Blood\n lead"),
           exposure = "Phthalates",
           outcome = "SRS")

ggdag(xx, text = FALSE, use_labels = "label")

ggdag_paths(xx, text = FALSE, use_labels = "label", shadow = TRUE)
ggdag_adjustment_set(xx, text = FALSE, use_labels = "label", shadow = TRUE)



yy<-dagify(A ~ B+C,
           E ~ A+D+C,
           C~B+D,
           exposure = "A",
           outcome = "E")
ggdag(yy)
ggdag_adjustment_set(yy, shadow = TRUE)
