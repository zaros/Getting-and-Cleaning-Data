# run_analysis.R
# @author Reza Rosli (zaros)
#
# Based on the instructions from this page:
# https://class.coursera.org/getdata-030/human_grading/view/courses/975114/assessments/3/submissions
#
# This script does the following:
#
# 1.  Merges the training and the test sets to create one data set.
# 2.  Extracts only the measurements on the mean and standard deviation for each measurement.
# 3.  Uses descriptive activity names to name the activities in the data set
# 4.  Appropriately labels the data set with descriptive variable names.
# 5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable
#     for each activity and each subject.


# Set the working directory to this script file's location
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

print(paste("Setting working directory to",this.dir))

dataDir <- "./UCI HAR Dataset/"

# If the dataset directory doesn't exist, download the dataset and create the directory
if (!file.exists(dataDir)) {
  print("Downloading dataset (this may take a while)")

  dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  tempFile <- tempfile()

  if (grepl("Windows", sessionInfo()$running)) {
    download.file(dataUrl,destfile = tempFile)
  }
  else {
     download.file(dataUrl, destfile = tempFile, method = "curl")
  }

  unzip(tempFile)
  rm(tempFile)
}

# load libraries
library(reshape2)

# read in the features.txt file to get the column names used in the data
features <- read.table(paste(dataDir,"features.txt",sep=""),header=FALSE)
features <- as.character(features[,"V2"])

# process the training dataset: combine the measurements with subject and activity information
print("Processing training data")
trainingData <- read.table(paste(dataDir,"train/X_train.txt",sep=""),header=FALSE,col.names=features)
trainingSubjects <- read.table(paste(dataDir,"train/subject_train.txt",sep=""),header=FALSE,col.names=c("Subject"))
trainingActivities <- read.table(paste(dataDir,"train/y_train.txt",sep=""),header=FALSE,col.names="Activity")
trainingData <- cbind(trainingSubjects,trainingActivities,trainingData)

# process the test dataset:  combine the measurements with subject and activity information
print("Processing test data")
testData <- read.table(paste(dataDir,"test/X_test.txt",sep=""),header=FALSE,col.names=features)
testSubjects <- read.table(paste(dataDir,"test/subject_test.txt",sep=""),header=FALSE,col.names=c("Subject"))
testActivities <- read.table(paste(dataDir,"test/y_test.txt",sep=""),header=FALSE,col.names="Activity",)
testData<-cbind(testSubjects,testActivities,testData)

# combine the datasets (STEP 1 is satisfied)
print("Combining training and test data")
combinedData <- rbind(testData,trainingData)

# cleanup: remove temporary objects
rm(trainingData,trainingSubjects,trainingActivities,testData,testSubjects,testActivities)

print("Extracting mean and standard deviation columns only")
# Extract only the measurements on the mean and standard deviation for each measurement:
# - get the list of the columns using regex() function
# - we'll need to substitute some characters to match the column names in the data frame
cols <- grep("(-mean|-std)\\(\\)",features,value=TRUE)
cols <- gsub("-",".",cols)
cols <- gsub("\\(\\)","..",cols)

# - we also want the Subject and Activity columns
cols <- c(c("Subject","Activity"),cols)

# - now just select the wanted columns
combinedData <- combinedData[,cols]

print("Performing transformations")
# make the Activity column in the dataset as Factors and give the factor levels descriptive names,
# according to the list provided in activity_labels.txt
activity_labels <- read.table(paste(dataDir,"activity_labels.txt",sep=""),header=FALSE)
combinedData$Activity <- as.factor(combinedData$Activity)
levels(combinedData$Activity)<-activity_labels[,"V2"]

# Rename column names of the dataset:
# - converting periods to underscores
names(combinedData)<-gsub("\\.\\.\\.","_",names(combinedData))
names(combinedData)<-gsub("\\.\\.$","",names(combinedData))
names(combinedData)<-gsub("\\.","_",names(combinedData))


# - I feel that the variable name clearer
# - if the Time & Frequency domain is labeled
# - if the axis identifier is right after the feature
names(combinedData)<-gsub("^t","Time",names(combinedData))
names(combinedData)<-gsub("^f","Freq",names(combinedData))
names(combinedData)<-gsub("_mean_X","_X_mean",names(combinedData))
names(combinedData)<-gsub("_mean_Y","_Y_mean",names(combinedData))
names(combinedData)<-gsub("_mean_Z","_Z_mean",names(combinedData))
names(combinedData)<-gsub("_std_X","_X_std",names(combinedData))
names(combinedData)<-gsub("_std_Y","_Y_std",names(combinedData))
names(combinedData)<-gsub("_std_Z","_Z_std",names(combinedData))

# - fix the double Body e.g. in fBodyBodyGyroMag (presumed typo in the data)
names(combinedData)<-gsub("BodyBody","Body",names(combinedData))

# At this point, STEPS 1-4 in the rubric should be satisfied, i.e.:
# 1. The training and the test sets are merged as one dataset
# 2. Only the measurements on the mean and standard deviation for each measurement is in the dataset
# 3. Activities in the dataset are descriptively named
# 4. Appropriately descriptive variable names

# STEP 5: Creating the tidy dataset with the average of each variable for each activity and each subject
print("Summarising data")
meltedData <-  melt(combinedData,id=c("Subject","Activity"),measure.vars = grep("std|mean",names(combinedData),value=TRUE))
tidyData <- dcast(meltedData,Subject+Activity~variable,mean)

# Write the tidyData to a txt file for submission:
# the file will be placed in an "output" directory,
# which will be created if it doesn't exist
if (!file.exists("./output")) {
  dir.create("./output")
}
write.table(tidyData,file="./output/tidyData.txt",row.names = FALSE)

print(paste("The processed data is now in",getwd(),"/output/tidyData.txt"))

# cleanup: remove temporary objects
rm(activity_labels,meltedData,cols,features)

print("Done.")
# END
