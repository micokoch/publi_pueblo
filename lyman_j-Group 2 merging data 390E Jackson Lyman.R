

#Import datasets from where you downloaded them. the path will be different
##Exposures and Outcomes

confounders2015 <- read.csv("C:/Users/Owner/Dropbox/UMASS/Teaching/EnvEpi_Spring2021/Datasets/confounders2015.csv")
("~/Downloads/confounders2015.csv")
group2 <- read.csv("~/Downloads/group2.csv")

names (group2)
names (confounders2015)
#Merge the two datasets together
data2 <- merge(group2,confounders2015,by="SEQN")

#Check where are your variables of interest in the datasets: Remember you only want to keep your exposure, outcome, and confounders
names(data2)
#Creating a new dataset only with what you need. For instance your exposure of interest is URXCOTT, and is in column 3. your outcome of interest KIQ022 is in column 25, and your confounders are in columns 40 to 44. I also kept the IDs "SEQN" in column 1
data.final=data2[,c(1,3,22,25,40:44)]

#Remove all missing data
data.final=na.omit(data.final)


#ONLY For groups who have depression as an outcome, you will need to construct the sum of DPQ010 to DPQ100
#Construct the sum for a depression total score
names(data.final)
#To construct the sum, instead of DPQ010 and DPQ100 below, put the rank of the column where these variables are situated (as done above to select the variables you need)
data.final$depression <- rowSums(data.final[,c(DPQ010:DPQ100)])