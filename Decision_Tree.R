library(dplyr)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#setting the path 
setwd("C:/DataScienceWithR/Assignments/Graded Assignments/Topic 11.2 -  Decison Trees")
#reading the file
BH<-read.csv("BH.csv")
#checking number of null values
summary(BH)
colSums(is.na(BH))
#they want only cus_employername
BH_NEW<-BH[,c("cus_employername","TARGET")]
#delete the missing values in target variable

missing_index<-which(is.na(BH_NEW$TARGET))
BH_NEW<-BH_NEW[-missing_index,]
#building the decision tree model
mod<-rpart(TARGET~+cus_employername,method = "class",data = BH_NEW,control = rpart.control(cp=0.002,
        minsplit = 2),parms = list(split="gini"))

#ploting the tree
fancyRpartPlot(mod)

options(scipen = 999)
#extracting the rules
asRules(mod)
#node 2 has target 0 with probability 81%
#node3 has target 1 with probability 88%

BH_NEW$Employer_group<-mod$where
View(BH_NEW)
#it has 2 for bad and 3 for good
#let's separate them in group1 and group2
#Group 1-> Consisting of all employer names where the bad rate is high (2) 
#Group 2 -> Consisting of all the employer names where bad rate is low (3)
BH_NEW$Employer_group<-ifelse(BH_NEW$Employer_group==2,"Group1","Group2")
View(BH_NEW)
