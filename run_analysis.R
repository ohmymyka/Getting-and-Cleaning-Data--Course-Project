##Getting and Cleaning Data Course Project
##RScript for merging the Human Activity Recognition Using Smartphones Data
##to one tidy dataset.

##Load all needed libraries

library(plyr)
library(dplyr)
library(reshape2)

#Merges the training and the test sets to create one data set.

      ##Merge the subject, X, and Y datasets into 1 dataset (traindata)
      setwd("./UCI HAR Dataset/train")
      subjectid<-read.table("subject_train.txt", header=FALSE,sep=" ")
      actid<-read.table("y_train.txt",header=FALSE,sep=" ")
      set<-read.table("X_train.txt", header=FALSE)
      traindata<-cbind(subjectid,actid,type=rep("train",nrow(subjectid)),set)
      
      ##Merge the subject, X, and Y datasets into 1 dataset (testdata)
      setwd("./UCI HAR Dataset/test")
      subjectid<-read.table("subject_test.txt", header=FALSE,sep=" ")
      actid<-read.table("y_test.txt",header=FALSE,sep=" ")
      set<-read.table("X_test.txt", header=FALSE)
      testdata<-cbind(subjectid,actid,type=rep("test",nrow(subjectid)),set)
      
      ##Merge the traindata and testdata
      combinedata<-rbind(traindata,testdata)
    
#Appropriately labels the data set with descriptive variable names. 

      ##Get the column labels from the features.txt
      setwd("./UCI HAR Dataset")
      collabels<-read.table("features.txt",header=FALSE)
      collabels<-as.vector(collabels[,2])
      
      ##Assign the column labels as column names to the tidy data 1
      addlabels<-c("subjectid","actid", "type")
      colall<-c(addlabels,collabels)
      colnames(combinedata)<-colall

#Uses descriptive activity names to name the activities in the data set

      ##Change the activity labels from numeric to descriptive
      activitylabels<-read.table("activity_labels.txt",header=FALSE)
      names(activitylabels)<-c("actid","activity")
      combinedata<-left_join(activitylabels,combinedata, by="actid")
      
#Extracts only the measurements on the mean and standard deviation for each measurement. 

      ##Select only the data that corresponds to the mean and sd of each measure
      meansddata <-combinedata[, grepl("subjectid|activity|type|mean|std",names(combinedata))]
      meansddata<-meansddata[,-(grep("meanFreq",names(meansddata)))]

      
#From the data set in step 4, creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject.
      
      ##Melts dataset into a narrow dataset
      varnames<-names(meansddata[,grep("mean|std",names(meansddata))])
      meansdmelt<-melt(meansddata,id=c("subjectid","activity","type"),measure.vars=varnames)
      
      ##Computes for the mean of type (train and test) across all subjects, activities and measures
      meansdcast<-dcast(meansdmelt,subjectid+activity~variable,mean)
      
      ##Converts to narrow database
      meansdmelt<-melt(meansdcast,id=c("subjectid","activity"),measure.vars=varnames,
                       variable.name = "measure",
                       value.name = "mean")
      write.table(meansdmelt,file="tidydata.txt", sep=" ",row.names=FALSE)