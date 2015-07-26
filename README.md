# Readme: Getting and Cleaning Data Course Project

## Introduction
This project is the course assignment for the ["Getting and Cleaning Data" course on Coursera](https://www.coursera.org/course/getdata). The purpose of this project is to demonstrate the student's ability to collect, work with, and clean a data set. 

The script written takes the source data set and performs the following tasks:
1.  Merges the training and the test sets to create one data set.
2.  Extracts only the measurements on the mean and standard deviation for each measurement.
3.  Uses descriptive activity names to name the activities in the data set
4.  Appropriately labels the data set with descriptive variable names.
5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

The data set generated by step 5 above is then exported as a text file to be uploaded as part of the course submission. 

## The Data Set
The data set referred to is the data collected from the accelerometers from the Samsung Galaxy S smartphone, as described at the following location:

Human Activity Recognition Using Smartphones Data Set 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

For the purpose of this project, a version of the data set which is hosted on the Coursera CDN is used instead. 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

See the accompanying Code Book for a description of the final data set.

## Dependencies
Ensure that the following R libraries are installed on your system:
- reshape2

## How to Use the Script
1. Clone the git repo from https://github.com/zaros/Getting-and-Cleaning-Data
2. Run the run_analysis.R
3. The script will automatically set the working directory to the script's directory and download the source data into a directory "UCI HAR Dataset". If the directory (and I assume the data) already exists, no files will be downloaded.
4. Two data sets will be generated: 
    - combinedData: result of steps 1-4 
    - tidyData: result of step 5, as a wide dataset
5. The script will create another directory "output" in the working directory and output the tidyData dataset a text file tidyData.txt  

And that's it!

Since you are probably reading this Readme file because you are peer-reviewing my code, I have made more descriptive comments than normal in the script. You can check the outputted the text file by reloading it in R in the following command:

tidyDataCheck <- read.table("./output/tidyData.txt",header=TRUE); head(tidyDataCheck)

If you do run the script you'll find that there are console messages telling you what the script is doing. It clutters up the code a bit, but since there are some long waits involved in the script execution, I put them in just to track what's going on.

## Notes & References
1. [David's personal course project FAQ](https://class.coursera.org/getdata-030/forum/thread?thread_id=37)
2. [Should the script "run_analysis.R" download and unzip files?](https://class.coursera.org/getdata-030/forum/thread?thread_id=238) - I did it as a personal exercise
3. [BodyBody AnybodyAnybody?](https://class.coursera.org/getdata-030/forum/thread?thread_id=225) - Apparently there are some typos in some column names in the data set. I believe this is indeed the case and fixed them accordingly in my tidyData set.
4. [What is the dimension of the final data set (whether to include meanFreq)](https://class.coursera.org/getdata-030/forum/thread?thread_id=228) - I only included columns specifically using "mean()" and "std()" in this script and ignored the rest. I gathered from this thread that the important thing here is knowing how to use the grep() function properly.
