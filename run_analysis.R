###################################

# Coursera Getting and Cleaning Data Course Project
# @author Klaus Astner

# This script will perform the following steps on the UCI HAR Dataset downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

###################################

# Start with clean environment
rm(list = ls());
currentWd <- getwd();


######################
# 1. Merge the training and the test sets to create one data set.
######################

setwd("data/UCI HAR Dataset/");

# Read the meta-data from files
features     <- read.table('./features.txt',header=FALSE); 
activityType <- read.table('./activity_labels.txt',header=FALSE); 
colnames(activityType)  <- c("activityId","activityType");


# Read the training data
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE); 
xTrain       <- read.table('./train/X_train.txt',header=FALSE); 
yTrain       <- read.table('./train/y_train.txt',header=FALSE); 

# Create the full training set by merging yTrain, subjectTrain, and xTrain
trainingData <- cbind(yTrain,subjectTrain,xTrain);

# Read in the test data
subjectTest <- read.table('./test/subject_test.txt',header=FALSE); 
xTest       <- read.table('./test/x_test.txt',header=FALSE); 
yTest       <- read.table('./test/y_test.txt',header=FALSE); 

# Create the full test set by merging the yTest, subjectTest and yTest data
testData <- cbind(yTest,subjectTest,xTest);

# merge training and test data, set the column names from feature data 
mergedData <- rbind(trainingData,testData);
colnames(mergedData) <- c("activityId", "subjectId", as.character(features[,2]))



######################
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
######################

# extract the columns names, to be used for selecting (grep) the std() and mean() columns
nn <- colnames(mergedData); 

# Create a selector with TRUE for Id, mean() & std() column names and FALSE for all  others
selector <- grepl(".*mean\\(\\)$",nn) | grepl(".*std\\(\\)$",nn) | grepl("Id",nn)

# Subset mergedData table based on the logicalVector to keep only desired columns
mergedData <- mergedData[selector==TRUE];


######################
# 3. Use descriptive activity names to name the activities in the data set
######################

# Merge the mergedData set with the acitivityType table to include descriptive activity names
mergedData <- merge(activityType,mergedData,by='activityId',all.x=TRUE);


######################
# 4. Appropriately label the data set with descriptive activity names. 
######################

# old state of columns names
nn  <- colnames(mergedData); 

# substiture columns names
nn <- gsub("^t","Time",nn);
nn <- gsub("^f","Frequeny",nn);
nn <- gsub("Mag","Magnitude",nn);
nn <- gsub("Acc","Acceleration",nn);
nn <- gsub("-mean\\(\\)","Mean",nn)
nn <- gsub("-std\\(\\)","StdDev",nn);

# reassign column names
colnames(mergedData) <- nn;

######################
# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
######################

# Create a new temporary table without the activityType column
tmpTable <- mergedData[,names(mergedData) != 'activityType'];

# Summarizing the tmpTable table to include just the mean of each variable for each activity and each subject
tidyDataSet <- aggregate(tmpTable[,names(tmpTable) != c('activityId','subjectId')],by=list(activityId=tmpTable$activityId,subjectId = tmpTable$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyDataSet <- merge(activityType,tidyDataSet,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyDataSet, './tidyDataSet.txt',row.names=TRUE,sep=' ');



######################

# and now reset everything
setwd(currentWd);
rm(list = ls());
