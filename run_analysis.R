# Working Directory is set, Variables Cleared
setwd("g:/COURSERA_TRACKS/GettingAndReadingData/Assignment")
rm(list=ls())
# Read and Prepare Data Step
# Reading Data from the Downloaded unzipped folder
activitylabel   <- read.table("./UCI HAR Dataset/activity_labels.txt")          
#Assign names for columns
colnames(activitylabel)  <- c("id","activitytype")                              
## read the training dataset and the test dataset
traininglabel   <- read.table("./UCI HAR Dataset/train/y_train.txt")           
testlabel       <- read.table("./UCI HAR Dataset/test/y_test.txt") 
# read the training and subject data
trainingdata    <- read.table("UCI HAR Dataset/train/X_train.txt")              
trainingsubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
# read the test and subject data
testdata        <- read.table("UCI HAR Dataset/test/X_test.txt")
testsubject     <- read.table("UCI HAR Dataset/test/subject_test.txt")
# assign column names to read data variables
colnames(traininglabel)  <- c("id")                                             
colnames(testlabel)      <- c("id")                                             
## Mergers
# merge traininglabel and activity label
traininglabel   <- merge(traininglabel, activitylabel, by.x="id", by.y="id", all=T)
# merge testlabel and activity label
testlabel       <- merge(testlabel, activitylabel, by.x="id", by.y="id", all=T)

# read the feature list
features        <- read.table("./UCI HAR Dataset/features.txt")                 

# assign column names from the features
colnames(trainingdata)          <-  features[,2] 
colnames(testdata)              <-  features[,2]
colnames(trainingsubject)       =  "subjectid"
colnames(testsubject)           =  "subjectid"

#bind columns to prepare Training Dataset
trainingdata    <- cbind(traininglabel, trainingdata, trainingsubject)

#bind columns to prepare Test Dataset
testdata        <- cbind(testlabel, testdata, testsubject)

# check for NA values
sum(is.na(trainingdata))
sum(is.na(testdata))

# combine all rows to prepare a single dataset which has both training and test data
FinalData       <- rbind(trainingdata, testdata)

#check for dimensions
dim(FinalData)

# PART 2
#Extract only the measurements on the mean and standard deviation for each measurement. 

columns <- colnames(FinalData)

# Lookfor mean and standard deviation in the column names
find_mean_std_only = ( grepl("id", columns) |grepl("activity..", columns) | grepl("subject..", columns) | grepl("-mean..", columns) | 
                         grepl("mean..-", columns) | grepl("meanFreq..", columns) | grepl("-std..", columns) | grepl("std..-", columns))


# Subsetting columns with mean and std deviation measurements
Mean_Std <- FinalData[find_mean_std_only]

# End Part 2
# Part 3
traininglabel   <- merge(traininglabel, activitylabel, by.x="id", by.y="id", all=T)
# merge testlabel and activity label
testlabel       <- merge(testlabel, activitylabel, by.x="id", by.y="id", all=T)



#Part 4
#Appropriately label the data set with descriptive variable names. 

# collect all column names 

newcolumns <- colnames(Mean_Std)

# Cleanning up

for (i in 1: length(newcolumns)) {
  
  newcolumns[i] = gsub("^(i)d", "ActivityId", newcolumns[i]);
  newcolumns[i] = gsub("\\()", "", newcolumns[i]);
  newcolumns[i] = gsub("-mean", "Mean", newcolumns[i]);
  newcolumns[i] = gsub("-std", "StdDevtn", newcolumns[i]);
  newcolumns[i] = gsub("[gG]yro", "Gyro", newcolumns[i]);
  newcolumns[i] = gsub("[gG]ravity", "Gravity", newcolumns[i]);
  newcolumns[i] = gsub("[bB]ody [bB]ody | [Bb]ody ", "Body", newcolumns[i]);
  newcolumns[i] = gsub("[aA]cc[mM]ag", "AccMagnitude", newcolumns[i]);
  newcolumns[i] = gsub("[jJ]erk[mM]ag", "JerkMagnitude", newcolumns[i]);
  newcolumns[i] = gsub("[gG]yro[mM]ag", "GyroMagnitude", newcolumns[i]);
  newcolumns[i] = gsub("^(t)", "Time", newcolumns[i]);
  newcolumns[i] = gsub("^(f)", "Frequency", newcolumns[i]);
  newcolumns[i] = gsub("^(a)", "A", newcolumns[i]);
  newcolumns[i] = gsub("^(s)", "S", newcolumns[i]);
}

# Reassign the proper variable names for the dataframe column names
colnames(Mean_Std) <- newcolumns

#Part 5 
#Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Remove the activitytype column 
noActivityType <- Mean_Std[,names(Mean_Std) != "Activitytype"]


tidyData    =  aggregate( noActivityType[,names(noActivityType) != c('ActivityId','Subjectid')],
                          by=list(ActivityId=noActivityType$ActivityId, SubjectId = noActivityType$Subjectid), mean);

colnames(activitylabel)  <- c("ActivityId","ActivityType")

# Merge tidy dataset to the activity label to form the final tidy dataset
TidyDataset   <- merge(tidyData, activitylabel, by="ActivityId", all=T)

#write this to a text file
write.table(TidyDataset[, names(TidyDataset) != 'Subjectid'], "./TidyDataset.txt", sep=",", row.names=FALSE)
