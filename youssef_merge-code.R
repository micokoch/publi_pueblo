#Import datasets
##Exposures and Outcomes
group2 <- read.csv("group2.csv")
##Confounders
confounders2015 <- read.csv("confounders2015.csv")

#Merge the two datasets together
data2 <- merge(group2,confounders2015,by="SEQN")

#Check where are your variables of interest in the datasets: Remember you only want to keep your exposure, outcome, and confounders
names(data2)
#Creating a new dataset only with what you need. For instance your exposure of interest is URXCOTT, and is in column 3. your outcome of interest KIQ022 is in column 25, and your confounders are in columns 40 to 44. I also kept the IDs "SEQN" in column 1
data.final=data2[,c(1,3,25,40:44)]

#Remove all missing data
data.final=na.omit(data.final)


#ONLY For groups who have depression as an outcome, you will need to construct the sum of DPQ010 to DPQ100
#Construct the sum for a depression total score
names(data.final)
#To construct the sum, instead of DPQ010 and DPQ100 below, put the rank of the column where these variables are situated (as done above to select the variables you need)
data.final$depression <- rowSums(data.final[,c(DPQ010:DPQ100)])
