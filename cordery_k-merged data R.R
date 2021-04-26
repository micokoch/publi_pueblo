#Import datasets from where you downloaded them. the path will be different
##Exposures and Outcomes

 group6 <- read.csv("group6.csv")
   View(group6)
 confounders2013 <- read.csv("confounders2013.csv")
   View(confounders2013)
#Merge the two datasets together
data2 <- merge(group6,confounders2013,by="SEQN")
names(group6)
names(confounders2013)
#Check where are your variables of interest in the datasets: Remember you only want to keep your exposure, outcome, and confounders
names(data2)
names(group6)
names(confounders2013)
#Creating a new dataset only with what you need. For instance your exposure of interest is URXCOTT, and is in column 3. your outcome of interest KIQ022 is in column 25, and your confounders are in columns 40 to 44. I also kept the IDs "SEQN" in column 1
data.final=data2[,c(1,3,32,36:40)]

#Remove all missing data
data.final=na.omit(data.final)
write.csv(data.final, "cordery_k-final_data.csv")

# names(confounders2013)
# [1] "V1" "V2" "V3" "V4" "V5" "V6"
#ONLY For groups who have depression as an outcome, you will need to construct the sum of DPQ010 to DPQ100
#Construct the sum for a depression total score 