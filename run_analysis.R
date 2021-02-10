#install.packages("dplyr")
#install.packages("data.table")
#Load Essential packages
library(data.table)
library(dplyr)

#Download UCI data files from the web
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destFile <- "projectDataset.zip"
if (!file.exists(destFile)) {
  download.file(url, destFile, method = "auto")
}

#Unzip the downloaded file
if (!file.exists("UCI_HAR_Dataset")) {
  unzip(destFile)
}
dateDownloaded <- date()

# set working directory
setwd("UCI HAR Dataset")

#reading Activity files
activityTest <- read.table("test/y_test.txt", header = F)
activityTrain <- read.table("train/y_train.txt" , header = F)
head(activityTest,50)
head(activityTrain,50)

#reading features files
featureTest <- read.table("test/X_test.txt", header = F)
featureTrain <- read.table("train/X_train.txt" , header = F)
head(featureTest,50)
head(featureTrain,50)

#reading subject files
subjectTest <- read.table("test/subject_test.txt", header = F)
subjectTrain <- read.table("train/subject_train.txt" , header = F)
head(subjectTest,50)
head(subjectTrain,50)

#read activity labels
activityLabels <- read.table("activity_labels.txt", header = F)
head(activityLabels,50)

#read feature name
featureName <- read.table("features.txt", header = F)
head(featureName,50)

#merge dataframes
featureDate <- rbind(featureTest,featureTrain)
subjectData <- rbind(subjectTest,subjectTrain)
activityData <- rbind(activityTest,activityTrain)
head(activityData)

###Renaming colums in activityData & activityLabels dataframes
names(activityData) <- "activityn"
names(activityData)
names(activityLabels) <- c("activityn","activity")
names(activityLabels)

####Get factor of Activity names
str(activityData)
str(activityLabels)
activity <- left_join(activityData, activityLabels, "activityn")[,2]
activity

#Rename subjectData columns
names(subjectData) <- "subject"
names(subjectData)

#Rename featureDate columns
names(featureDate) <- featureName[,2]
names(featureDate)

# Create one large Dataset with only these variables: subjectData,activity,featureDate
dataSet <- cbind(subjectData, activity)
dataSet <- cbind(dataSet, featureDate)
str(dataSet)

#Create New datasets by extracting only the measurements on the mean and standard deviation for each measurement
subFeatureName <- featureName$V2[grep("mean\\(\\)|std\\(\\)",featureName$V2)]
columnName <- c("subject","activity",subFeatureName)
dataSet <- select(dataSet, columnName)
dim(dataSet)
str(dataSet)

#Rename the columns of the large dataset using more descriptive activity names
names(dataSet) <- gsub("^t","time",names(dataSet))
names(dataSet) <- gsub("^f","frequence",names(dataSet))
names(dataSet)<-gsub("Acc", "Accelerometer", names(dataSet))
names(dataSet)<-gsub("Gyro", "Gyroscope", names(dataSet))
names(dataSet)<-gsub("Mag", "Magnitude", names(dataSet))
names(dataSet)<-gsub("BodyBody", "Body", names(dataSet))

#Create a second, independent tidy data set with the average of each variable for each activity and each subject
str(dataSet)
secondDataSet <- aggregate(. ~subject + activity, dataSet, mean)
str(secondDataSet)
dim(secondDataSet)
head(secondDataSet[,c(1,2)],100)
secondDataSet <- arrange(secondDataSet,subject,activity)
