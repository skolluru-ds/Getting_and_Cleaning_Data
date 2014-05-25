## The following is the directory where the assignment information resides.
## setwd("F:/Coursera/DataScienceTrack/GettingNCleaningData/ProgAssessment/")

## Step1. Merges the training and the test sets to create one data set.
trainData <- read.table("./UCIHAR_Dataset/train/X_train.txt")
dim(trainData) ## The dimension is 7352*561
head(trainData)
trainLabel <- read.table("./UCIHAR_Dataset/train/y_train.txt")
table(trainLabel)
trainSubject <- read.table("./UCIHAR_Dataset/train/subject_train.txt")
testData <- read.table("./UCIHAR_Dataset/test/X_test.txt")
dim(testData) ## The dimension is 2947*561
testLabel <- read.table("./UCIHAR_Dataset/test/y_test.txt") 
table(testLabel) 
testSubject <- read.table("./UCIHAR_Dataset/test/subject_test.txt")
joinData <- rbind(trainData, testData)
dim(joinData) ## The dimension is 10299*561
joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel) ## The dimension is 10299*1
joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject) ## The dimension is 10299*1

## Step2. Extracts only the measurements on the mean and standard 
## deviation for each measurement. 
features <- read.table("./UCIHAR_Dataset/features.txt")
dim(features)  ## The dimension is 561*2
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) ## The result should be 66
joinData <- joinData[, meanStdIndices]
dim(joinData) ## The dimension is 10299*66
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) ## remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) ## capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) ## capitalize S
names(joinData) <- gsub("-", "", names(joinData)) ## remove "-" in column names 

## Step3. Uses descriptive activity names to name the activities in 
## the data set
activity <- read.table("./UCIHAR_Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

## Step4. Appropriately labels the data set with descriptive activity 
## names. 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData) # The dimension is 10299*68
## write the 1st dataset
write.table(cleanedData, "merged_data.txt") 

## Step5. Creates a second, independent tidy data set with the average of 
## each variable for each activity and each subject. 
subjectLen <- length(table(joinSubject)) # The length should be 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
    for(j in 1:activityLen) {
        result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
        result[row, 2] <- activity[j, 2]
        bool1 <- i == cleanedData$subject
        bool2 <- activity[j, 2] == cleanedData$activity
        result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
        row <- row + 1
    }
}
head(result)
## write the 2nd dataset
write.table(result, "tidydata_with_means.txt") 

## Check the results written to the file.
## data <- read.table("./tidydata_with_means.txt")
## data[, 1:5]
