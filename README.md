---
output: html_document
---
# Getting and Cleaning Data - Assignment
### Edwig Aloysius Noronha
### 3 April 2019

This is the solution to the Peer-graded Assignment: Getting and Cleaning Data Course Project. the solution consists of the following scripts:

### run_analysis.R
This R script does the following:

* Downloads and unzips the dataset
* Loads activities and features
* Extracts only the measurements on the mean and standard deviation
* Loads the training and the test sets keeping only the mean and standard deviation for each measurement
* Merges the training and the test sets to create one data set
* Appropriately labels the data set with descriptive variable names.
* Uses descriptive activity names to name the activities in the data set
* creates a second, independent tidy data set with the average of each variable for each activity and each subject
* writes the tidy data set to disc

### tidy.txt
The end result of the analysis.

### CodeBook.md
A code book that describes the variables, the data, and any transformations performed to clean up the data.
