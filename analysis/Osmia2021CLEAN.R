
library(ggplot2)
library(plyr)
library("survival")
library("survminer")

setwd("~/Library/CloudStorage/Box-Box/Rachel Fordyce/github")

data <- read.csv("Data2021Merged10-28-22.csv")
#remove samples with missing data
data <- data[-which(data$death.date == "missing"),]
#make sure data in df are correct class
data$Date.closed <- as.Date(data$Date.closed, format = "%m/%d/%y")
data$Date.opened <- as.Date(data$Date.opened, format = "%m/%d/%y")
data$sample.date <- as.Date(data$sample.date, format = "%m/%d/%y")
data$death.date <- as.Date(data$death.date, format = "%m/%d/%y")
data$Treatment <- as.factor(data$Treatment.Group)#rename column to conform with code


# data1 <- read.csv("DataMergedForAnalysis6-09-22.csv")
# data2 <- read.csv("OsmiaEmergence2021.csv")
# data <- merge(data1, data2, by="Sample.ID")
# write.csv(data, file = "Data2021Merged10-28-22.csv")


