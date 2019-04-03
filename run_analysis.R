library(reshape2)
library(plyr)
library(dplyr)

filename <- "dataset.zip"

## Downloads and unzips the dataset:
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# Loads activities and features
activities <- read.table("UCI HAR Dataset/activity_labels.txt")
activities[,2] <- as.character(activities[,2])

features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

# Extracts only the measurements on the mean and standard deviation for each measurement
extractredFeatures <- grep(".*mean.*|.*std.*", features[,2])
extractredFeatures.names <- features[extractredFeatures,2]
extractredFeatures.names <- gsub('-mean', 'Mean', extractredFeatures.names)
extractredFeatures.names <- gsub('-std', 'Std', extractredFeatures.names)
extractredFeatures.names <- gsub('[-()]', '', extractredFeatures.names)

# Loads the training and the test sets
traindata <- read.table("UCI HAR Dataset/train/X_train.txt")[extractredFeatures]
trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
traindataset <- cbind(trainSubjects, trainActivities, traindata)

testdata <- read.table("UCI HAR Dataset/test/X_test.txt")[extractredFeatures]
testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
testdataset <- cbind(testSubjects, testActivities, testdata)

# Merges the training and the test sets to create one data set
finaldata <- rbind(traindataset, testdataset)
# Appropriately labels the data set with descriptive variable names
colnames(finaldata) <- c("subject", "activity", extractredFeatures.names)

# Uses descriptive activity names to name the activities in the data set
finaldata$activity <- mapvalues(finaldata$activity, from=as.character(activities[,1]), to=as.character(activities[,2]))
finaldata$activity <- as.factor(finaldata$activity)

#creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject
finaldata.melted <- melt(finaldata, id = c("subject", "activity"))
groupeddata <- group_by(finaldata.melted, subject,activity,variable)
summarydata <- summarize(groupeddata, avg = mean(value, na.rm = T))

#writes the tidy data set to disc
write.table(summarydata, "tidy.txt", row.names = FALSE, quote = FALSE)
