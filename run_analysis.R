# Load Packages ----
library(reshape2)
library(dplyr)
require(janitor)
library(tidyr)

# Load data set ----

# get data set from web
DatasetDir <- "data/"
DatasetUrl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip"
DatasetFilename <- "dataSet.zip"
DatasetDFn <- paste(DatasetDir, "/", "dataSet.zip", sep = "")
datasetDir <- "data/dataset"


if (!file.exists(DatasetDir)) {
  dir.create(DatasetDir)
  download.file(url = DatasetUrl, destfile = DatasetDFn)
}
# unzip data set
if (!file.exists(datasetDir)) {
  dir.create(datasetDir)
  unzip(zipfile = DatasetDFn, exdir = datasetDir)
}

# Load data set

## features data set
features <- read.table(paste(sep = "", datasetDir, "/UCI HAR Dataset/features.txt"))
## train data set
x_train <- read.table(paste(sep = "", datasetDir, "/UCI HAR Dataset/train/X_train.txt"))
y_train <- read.table(paste(sep = "", datasetDir, "/UCI HAR Dataset/train/Y_train.txt"))
s_train <- read.table(paste(sep = "", datasetDir, "/UCI HAR Dataset/train/subject_train.txt"))

## test data set
x_test <- read.table(paste(sep = "", datasetDir, "/UCI HAR Dataset/test/X_test.txt"))
y_test <- read.table(paste(sep = "", datasetDir, "/UCI HAR Dataset/test/Y_test.txt"))
s_test <- read.table(paste(sep = "", datasetDir, "/UCI HAR Dataset/test/subject_test.txt"))

## activity labels 
activitylabels <- read.table(paste(sep = "", datasetDir, "/UCI HAR Dataset/activity_labels.txt"))

# Trasnform data set ----
# rename data set
## test data set
names(s_test) <- "subjectIdentifier"
names(x_test) <- make.names(features[,2], unique=TRUE)
names(y_test) <- "activity"

## train data set
names(s_train) <- "subjectIdentifier"
names(x_train) <- make.names(features[,2], unique=TRUE)
names(y_train) <- "activity"


# Select the indices of the features that have mean() and std() in the name
filter_features = features[grepl("\\-mean\\(\\)|\\-std\\(\\)",features[,2], perl=TRUE),1]


# 1 Merges the training and the test sets to create one data set. ----
oneDataset <- rbind(data.frame(s_test
                              , x_test
                              , y_test)
                   , data.frame(s_train
                                , x_train
                                , y_train))

# 2 Extracts only the measurements on the mean and standard deviation for each measurement ----
oneDataset <- oneDataset %>% 
  select(subjectIdentifier, filter_features, activity)

# 3 Uses descriptive activity names to name the activities in the data set ----
oneDataset$activity <- activitylabels[oneDataset$activity,2]

# 4 Appropriately labels the data set with descriptive variable names. ----

## Rename vars
names(oneDataset)[names(oneDataset) == 'V2'] <- 'activityDescription'
names(oneDataset)[names(oneDataset) == 'tBodyAcc.mean...X'] <- 'timeDomainBodyAccelerometerMeanXAxis'
names(oneDataset)[names(oneDataset) == 'tBodyAcc.mean...Y'] <- 'timeDomainBodyAccelerometerMeanYAxis'
names(oneDataset)[names(oneDataset) == 'tBodyAcc.mean...Z'] <- 'timeDomainBodyAccelerometerMeanZAxis'
names(oneDataset)[names(oneDataset) == 'tBodyAcc.std...X'] <- 'timeDomainBodyAccelerometerStdXAxis'
names(oneDataset)[names(oneDataset) == 'tBodyAcc.std...Y'] <- 'timeDomainBodyAccelerometerStdYAxis'
names(oneDataset)[names(oneDataset) == 'tBodyAcc.std...Z'] <- 'timeDomainBodyAccelerometerStdZAxis'
names(oneDataset)[names(oneDataset) == 'tGravityAcc.mean...X'] <- 'timeDomainGravityAccelerometerMeanXAxis'
names(oneDataset)[names(oneDataset) == 'tGravityAcc.mean...Y'] <- 'timeDomainGravityAccelerometerMeanYAxis'
names(oneDataset)[names(oneDataset) == 'tGravityAcc.mean...Z'] <- 'timeDomainGravityAccelerometerMeanZAxis'
names(oneDataset)[names(oneDataset) == 'tGravityAcc.std...X'] <- 'timeDomainGravityAccelerometerStdXAxis'
names(oneDataset)[names(oneDataset) == 'tGravityAcc.std...Y'] <- 'timeDomainGravityAccelerometerStdYAxis'
names(oneDataset)[names(oneDataset) == 'tGravityAcc.std...Z'] <- 'timeDomainGravityAccelerometerStdZAxis'
names(oneDataset)[names(oneDataset) == 'tBodyAccJerk.mean...X'] <- 'timeDomainBodyAccelerometerJerkMeanXAxis'
names(oneDataset)[names(oneDataset) == 'tBodyAccJerk.mean...Y'] <- 'timeDomainBodyAccelerometerJerkMeanYAxis'
names(oneDataset)[names(oneDataset) == 'tBodyAccJerk.mean...Z'] <- 'timeDomainBodyAccelerometerJerkMeanZAxis'
names(oneDataset)[names(oneDataset) == 'tBodyAccJerk.std...X'] <- 'timeDomainBodyAccelerometerJerkStdXAxis'
names(oneDataset)[names(oneDataset) == 'tBodyAccJerk.std...Y'] <- 'timeDomainBodyAccelerometerJerkStdYAxis'
names(oneDataset)[names(oneDataset) == 'tBodyAccJerk.std...Z'] <- 'timeDomainBodyAccelerometerJerkStdZAxis'
names(oneDataset)[names(oneDataset) == 'tBodyGyro.mean...X'] <- 'timeDomainBodyGyroscopeMeanXAxis'
names(oneDataset)[names(oneDataset) == 'tBodyGyro.mean...Y'] <- 'timeDomainBodyGyroscopeMeanYAxis'
names(oneDataset)[names(oneDataset) == 'tBodyGyro.mean...Z'] <- 'timeDomainBodyGyroscopeMeanZAxis'
names(oneDataset)[names(oneDataset) == 'tBodyGyro.std...X'] <- 'timeDomainBodyGyroscopeStdXAxis'
names(oneDataset)[names(oneDataset) == 'tBodyGyro.std...Y'] <- 'timeDomainBodyGyroscopeStdYAxis'
names(oneDataset)[names(oneDataset) == 'tBodyGyro.std...Z'] <- 'timeDomainBodyGyroscopeStdZAxis'
names(oneDataset)[names(oneDataset) == 'tBodyGyroJerk.mean...X'] <- 'timeDomainBodyGyroscopeJerkMeanXAxis'
names(oneDataset)[names(oneDataset) == 'tBodyGyroJerk.mean...Y'] <- 'timeDomainBodyGyroscopeJerkMeanYAxis'
names(oneDataset)[names(oneDataset) == 'tBodyGyroJerk.mean...Z'] <- 'timeDomainBodyGyroscopeJerkMeanZAxis'
names(oneDataset)[names(oneDataset) == 'tBodyGyroJerk.std...X'] <- 'timeDomainBodyGyroscopeJerkStdXAxis'
names(oneDataset)[names(oneDataset) == 'tBodyGyroJerk.std...Y'] <- 'timeDomainBodyGyroscopeJerkStdYAxis'
names(oneDataset)[names(oneDataset) == 'tBodyGyroJerk.std...Z'] <- 'timeDomainBodyGyroscopeJerkStdZAxis'
names(oneDataset)[names(oneDataset) == 'tBodyAccMag.mean..'] <- 'timeDomainBodyAccelerometerMeanMagnitude'
names(oneDataset)[names(oneDataset) == 'tBodyAccMag.std..'] <- 'timeDomainBodyAccelerometerStdMagnitude'
names(oneDataset)[names(oneDataset) == 'tGravityAccMag.mean..'] <- 'timeDomainGravityAccelerometerMeanMagnitude'
names(oneDataset)[names(oneDataset) == 'tGravityAccMag.std..'] <- 'timeDomainGravityAccelerometerStdMagnitude'
names(oneDataset)[names(oneDataset) == 'tBodyAccJerkMag.mean..'] <- 'timeDomainBodyAccelerometerJerkMeanMagnitude'
names(oneDataset)[names(oneDataset) == 'tBodyAccJerkMag.std..'] <- 'timeDomainBodyAccelerometerJerkStdMagnitude'
names(oneDataset)[names(oneDataset) == 'tBodyGyroMag.mean..'] <- 'timeDomainBodyGyroscopeMeanMagnitude'
names(oneDataset)[names(oneDataset) == 'tBodyGyroMag.std..'] <- 'timeDomainBodyGyroscopeStdMagnitude'
names(oneDataset)[names(oneDataset) == 'tBodyGyroJerkMag.mean..'] <- 'timeDomainBodyGyroscopeJerkMeanMagnitude'
names(oneDataset)[names(oneDataset) == 'tBodyGyroJerkMag.std..'] <- 'timeDomainBodyGyroscopeJerkStdMagnitude'
names(oneDataset)[names(oneDataset) == 'fBodyAcc.mean...X'] <- 'freqDomainBodyAccelerometerMeanXAxis'
names(oneDataset)[names(oneDataset) == 'fBodyAcc.mean...Y'] <- 'freqDomainBodyAccelerometerMeanYAxis'
names(oneDataset)[names(oneDataset) == 'fBodyAcc.mean...Z'] <- 'freqDomainBodyAccelerometerMeanZAxis'
names(oneDataset)[names(oneDataset) == 'fBodyAcc.std...X'] <- 'freqDomainBodyAccelerometerStdXAxis'
names(oneDataset)[names(oneDataset) == 'fBodyAcc.std...Y'] <- 'freqDomainBodyAccelerometerStdYAxis'
names(oneDataset)[names(oneDataset) == 'fBodyAcc.std...Z'] <- 'freqDomainBodyAccelerometerStdZAxis'
names(oneDataset)[names(oneDataset) == 'fBodyAccJerk.mean...X'] <- 'freqDomainBodyAccelerometerJerkMeanXAxis'
names(oneDataset)[names(oneDataset) == 'fBodyAccJerk.mean...Y'] <- 'freqDomainBodyAccelerometerJerkMeanYAxis'
names(oneDataset)[names(oneDataset) == 'fBodyAccJerk.mean...Z'] <- 'freqDomainBodyAccelerometerJerkMeanZAxis'
names(oneDataset)[names(oneDataset) == 'fBodyAccJerk.std...X'] <- 'freqDomainBodyAccelerometerJerkStdXAxis'
names(oneDataset)[names(oneDataset) == 'fBodyAccJerk.std...Y'] <- 'freqDomainBodyAccelerometerJerkStdYAxis'
names(oneDataset)[names(oneDataset) == 'fBodyAccJerk.std...Z'] <- 'freqDomainBodyAccelerometerJerkStdZAxis'
names(oneDataset)[names(oneDataset) == 'fBodyGyro.mean...X'] <- 'freqDomainBodyGyroscopeMeanXAxis'
names(oneDataset)[names(oneDataset) == 'fBodyGyro.mean...Y'] <- 'freqDomainBodyGyroscopeMeanYAxis'
names(oneDataset)[names(oneDataset) == 'fBodyGyro.mean...Z'] <- 'freqDomainBodyGyroscopeMeanZAxis'
names(oneDataset)[names(oneDataset) == 'fBodyGyro.std...X'] <- 'freqDomainBodyGyroscopeStdXAxis'
names(oneDataset)[names(oneDataset) == 'fBodyGyro.std...Y'] <- 'freqDomainBodyGyroscopeStdYAxis'
names(oneDataset)[names(oneDataset) == 'fBodyGyro.std...Z'] <- 'freqDomainBodyGyroscopeStdZAxis'
names(oneDataset)[names(oneDataset) == 'fBodyAccMag.mean..'] <- 'freqDomainBodyAccelerometerMeanMagnitude'
names(oneDataset)[names(oneDataset) == 'fBodyAccMag.std..'] <- 'freqDomainBodyAccelerometerStdMagnitude'
names(oneDataset)[names(oneDataset) == 'fBodyBodyAccJerkMag.mean..'] <- 'freqDomainBodyAccelerometerJerkMeanMagnitude'
names(oneDataset)[names(oneDataset) == 'fBodyBodyAccJerkMag.std..'] <- 'freqDomainBodyAccelerometerJerkStdMagnitude'
names(oneDataset)[names(oneDataset) == 'fBodyBodyGyroMag.mean..'] <- 'freqDomainBodyGyroscopeMeanMagnitude'
names(oneDataset)[names(oneDataset) == 'fBodyBodyGyroMag.std..'] <- 'freqDomainBodyGyroscopeStdMagnitude'
names(oneDataset)[names(oneDataset) == 'fBodyBodyGyroJerkMag.mean..'] <- 'freqDomainBodyGyroscopeJerkMeanMagnitude'
names(oneDataset)[names(oneDataset) == 'fBodyBodyGyroJerkMag.std..'] <- 'freqDomainBodyGyroscopeJerkStdMagnitude'

tidy <- tbl_df(oneDataset)
gatherTidy <- gather(tidy, measurement, value, -subjectIdentifier,-activity)

groupTidy <- group_by(gatherTidy,subjectIdentifier,activity,measurement)


# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. ----
summaryTidy <- summarize(groupTidy, value=mean(value))

summaryTidy
