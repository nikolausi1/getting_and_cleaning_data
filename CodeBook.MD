## Getting and Cleaning Data Project

Klaus Astner

### Description
Further information about the variables, data and transformations used in the "Getting anc Cleaning Data" course.

### Source and Script
The data for the project can be found at: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

The source code can be found at: https://github.com/nikolausi1/getting_and_cleaning_data


### Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Attribute Information
For each record in the dataset it is provided: 
- Tri-axial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Tri-axial angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- An activity label. 
- An identifier of the subject who carried out the experiment.

### Section 1. Merge the training and the test sets to create one data set.
After setting the source directory data is read in:
meta-data
- features.txt
- activity_labels.txt

training data
- subject_train.txt
- x_train.txt
- y_train.txt

test data
- subject_test.txt
- x_test.txt
- y_test.txt

The data are read in, merged and colums names are assigned.


## Section 2. Extract only the measurements on the mean and standard deviation for each measurement. 
Create a selector with TRUE for Id, mean() & std() column names and FALSE for all  others
This selector then helps in sub-setting the data set

## Section 3. Use descriptive activity names to name the activities in the data set
The activity type (see meta-data above) is read in and assigned as "speaking" name in a separate column.

## Section 4. Appropriately label the data set with descriptive activity names.
This section allows to change the columns names with the help of global substitute (gsub) and selecting with regular expressions.

## Section 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
Here we have to remove activity type first, calculate the average (mean) for all columns except activityId and subjectID and to re-assign activityType. Problematic is the subsetting, excluding 
columns and listing results. With activity type this did not work.

## Clean
Of course we clean the memory and jump back to home directory.
