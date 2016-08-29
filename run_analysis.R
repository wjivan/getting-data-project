#Clear workspace and change directory
rm(list=ls())
setwd("/Users/wenjian/Desktop/R programming/Cleaning data/Project")

        ##Q1. Merges the training and the test sets to create one data set.
#Define all file paths
#File paths for 561 variables and 6 activities
varname<-file.path("/Users/wenjian/Desktop/R programming/Cleaning data/UCI HAR Dataset/features.txt")
actname<-file.path("/Users/wenjian/Desktop/R programming/Cleaning data/UCI HAR Dataset/activity_labels.txt")

#File paths for test data sets. Input analysis is not used
ft<-file.path("/Users/wenjian/Desktop/R programming/Cleaning data/UCI HAR Dataset/test/X_test.txt")
ftlabel<-file.path("/Users/wenjian/Desktop/R programming/Cleaning data/UCI HAR Dataset/test/y_test.txt")
ftsub<-file.path("/Users/wenjian/Desktop/R programming/Cleaning data/UCI HAR Dataset/test/subject_test.txt")

#File paths for training data sets. Input analysis is not used
ftr<-file.path("/Users/wenjian/Desktop/R programming/Cleaning data/UCI HAR Dataset/train/X_train.txt")
ftrlabel<-file.path("/Users/wenjian/Desktop/R programming/Cleaning data/UCI HAR Dataset/train/y_train.txt")
ftrsub<-file.path("/Users/wenjian/Desktop/R programming/Cleaning data/UCI HAR Dataset/train/subject_train.txt")

#Read and save subject file for test
subtest<-read.table(ftsub)
#Rename column to subject
names(subtest)<-"subject"

#Read and save activity labels for test
labeltest<-read.table(ftlabel)
names(labeltest)<-"activitycode"

#Read main test results
test<-read.table(ft)

#Read 561 variable names
var<-read.table(varname)

#Save 561 variable names in colnames
colnames<-var$V2

#Rename columns with variable names
names(test)<-colnames

#Merge by column activity code, subjects and main test results
test<-cbind.data.frame(subtest,labeltest,test)

#Repeat same procedure for training data set
subtrain<-read.table(ftrsub)
names(subtrain)<-"subject"

labeltrain<-read.table(ftrlabel)
names(labeltrain)<-"activitycode"

train<-read.table(ftr)
names(train)<-colnames
train<-cbind.data.frame(subtrain,labeltrain,train)

#Merge both data sets
combined<-rbind.data.frame(test,train)

        ##Q2. Extracts only the measurements on the mean and standard deviation for each measurement.
#Create a search to filter out mean and std variables
sift<-grepl("mean\\(\\)|std\\(\\)",names(combined))
#Preserve activity code and subject columns
sift[1:2]<-c(TRUE,TRUE)
combined<-combined[,which(sift)]

        ##Q3.Uses descriptive activity names to name the activities in the data set
#Match activity codes with descriptive activity labels
activities<-read.table(actname)
names(activities)<-c("activitycode","activitynames")
combined<-merge(activities,combined,by="activitycode")

        #4. Appropriately labels the data set with descriptive variable names.
#Clean up data variable names, using lower case and removing unwanted symbols
rename<-names(combined)
rename<-tolower(rename)
rename<-gsub("\\(\\)","",rename)
rename<-gsub("-","",rename)
rename<-gsub("^t","time",rename)
rename<-gsub("^f","freq",rename)
rename<-gsub("(body)+","body",rename)
names(combined)<-rename

        #5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#dplyr package to help summarise data (mean) by each subject and activity names
library(dplyr)
tidy<-copy(combined)
tidy$activitycode<-NULL
tidy<-group_by(tidy,subject,activitynames) %>% summarise_each(funs(mean))