setwd("C:/Anthony_Wynn/Docs/R/R_Lang/hopkins/GettingAndCleaningData/CourseProjectj")
getwd()

# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#   
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Here are the data for the project:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following. 
# 
# Step-1: Download, Extract and Unzip file to working dir.
# Step-2: Merges the training and the test sets to create one data set.
# Step-3: Extracts only the measurements on the mean and standard deviation for each measurement. 
# Step-4: Uses descriptive activity names to name the activities in the data set
# Step-5:Appropriately labels the data set with descriptive activity names. 
# Step-6:Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#You can read a zipped file directly into R, e.g.:

#### Step - 1 : Download, Extract and Unzip file to working dir. #####
url= ("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
#create tmp_dir for later unzip
temp_dir=tempdir()
# create the temp zip file_holder + its path with temp_dir 
(file_holder=tempfile(tmpdir=temp_dir, fileext=".zip")  )
#download file to temp placeholder for unzip
(download.file(url, file_holder)  )
# unzip file to current working dir 
unzip(file_holder, files=NULL, exdir=getwd(), overwrite=TRUE)


#### Step - 2 : Read then merges the training and the test sets to create one data set.  ####

#2a. read train dataset(train, label, subject)
trainingSetDF    <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainingLabelsDF <- read.table("./UCI HAR Dataset/train/y_train.txt")
trainSubjectIdDF <- read.table("./UCI HAR Dataset/train/subject_train.txt")


#2b. read test dataset(train, label, subject)
testSetDF <- read.table("./UCI HAR Dataset/test/X_test.txt")
testLabelsDF <- read.table("./UCI HAR Dataset/test/y_test.txt") 
testSubjectIdDF <- read.table("./UCI HAR Dataset/test/subject_test.txt")


#2c. merge/rbind train with test dataset(*Set, label, subject)
joinDataSet <- rbind(trainingSetDF, testSetDF)
joinLabel <- rbind(trainingLabelsDF, testLabelsDF)
joinSubject <- rbind(trainSubjectIdDF, testSubjectIdDF)

#### Step - 3 : Extracts only the measurements on the mean and standard deviation for each measurement. ####
featuresFunctDF <- read.table("./UCI HAR Dataset/features.txt")
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", featuresFunctDF[, 2])
#meanStdIndices <- grep("mean|std", featuresFunctDF[, 2])
joinDataSet <- joinDataSet[, meanStdIndices]  #for coln header
names(joinDataSet) <- gsub("\\(\\)", "", featuresFunctDF[meanStdIndices, 2])  # replace "()" with blank
names(joinDataSet) <- gsub("mean", "Mean", names(joinDataSet))            # Replace "mean" with "Mean"
names(joinDataSet) <- gsub("std", "Std", names(joinDataSet))              # Replace "std" with "Std"
names(joinDataSet) <- gsub("-", "", names(joinDataSet))                   # replace "-" in column names with blank 

#### Step - 4 : Uses descriptive activity names to name the activities in the data set #####
activityLabelsDF <- read.table("./UCI HAR Dataset/activity_labels.txt")
activityLabelsDF[, 2] <- tolower(gsub("_", "", activityLabelsDF[, 2]))
substr(activityLabelsDF[2, 2], 8, 8) <- toupper(substr(activityLabelsDF[2, 2], 8, 8))
substr(activityLabelsDF[3, 2], 8, 8) <- toupper(substr(activityLabelsDF[3, 2], 8, 8))
activityLabel <- activityLabelsDF[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activityName"   #coln label

#### Step - 5. Appropriately labels the data set with descriptive activity names.  ####
names(joinSubject) <- "testSubjectID"
outputDescActivity <- cbind(joinSubject, joinLabel, joinDataSet)
write.csv(outputDescActivity, "ouputActivityDataSet.csv", row.names=T) # write out the 1st dataset
write.table(outputDescActivity, "ouputActivityDataSet.txt") # write out the 1st dataset

#### Step - 6. Creates a second, independent tidy data set with the average of ####
#### each variable for each activity and each subject. ####
(subjectLen <- length(table(joinSubject)) ) #chk=30L
(activityLen <- dim(activityLabelsDF)[1] )          #chk=6L 

columnLen <- dim(outputDescActivity)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen)
result <- as.data.frame(result)
colnames(result) <- colnames(outputDescActivity)
row = 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activityLabelsDF[j, 2]
    bool1 <- i == outputDescActivity$testSubjectID
    bool2 <- activityLabelsDF[j, 2] == outputDescActivity$activityName
    result[row, 3:columnLen] <- colMeans(outputDescActivity[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)
write.csv(result, "AvgSecondTidyDataSet.csv", row.names=T) # write out the 2nd dataset
write.table(result, "AvgSecondTidyDataSet.txt") # write out the 2nd dataset

