## Merging the training and the test sets to create one data set and
## reading subject training data
subject_train = read.table("UCI HAR Dataset/train/subject_train.txt", col.names=c("subject_id"))

## assigning row number as the values of ID column
subject_train$ID <- as.numeric(rownames(subject_train))

## reading training data
X_train = read.table("UCI HAR Dataset/train/X_train.txt")

## assigning row number as the values of ID column
X_train$ID <- as.numeric(rownames(X_train))

## reading activity training data
y_train = read.table("UCI HAR Dataset/train/y_train.txt", col.names=c("activity_id"))  

## assigning row number as the values of ID column
y_train$ID <- as.numeric(rownames(y_train))

## merging subject_train and y_train to train
train <- merge(subject_train, y_train, all=TRUE)

## merging train and X_train
train <- merge(train, X_train, all=TRUE)

## reading subject training data
subject_test = read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject_id"))

## assigning row number as the values of ID column
subject_test$ID <- as.numeric(rownames(subject_test))

## reading testing data
X_test = read.table("UCI HAR Dataset/test/X_test.txt")

## assigning row number as the values of ID column
X_test$ID <- as.numeric(rownames(X_test))

## read activity testing data
y_test = read.table("UCI HAR Dataset/test/y_test.txt", col.names=c("activity_id"))  

## assigning row number as the values of ID column
y_test$ID <- as.numeric(rownames(y_test))

## merging subject_test and y_test to train
test <- merge(subject_test, y_test, all=TRUE) 

## merging test and X_test
test <- merge(test, X_test, all=TRUE) 
 
## combining train and test
Dataset1 <- rbind(train, test)


## Extracting only the measurements on the mean and standard deviation for 
## each measurement.
features = read.table("UCI HAR Dataset/features.txt", col.names=c("feature_id", "feature_label"),)

## Extracting only the measurements on the mean and standard deviation for 
## each measurement. 
selected_features <- features[grepl("mean\\(\\)", features$feature_label) | grepl("std\\(\\)", features$feature_label), ]
dataset2 <- dataset1[, c(c(1, 2, 3), selected_features$feature_id + 3) ]

# Using descriptive activity names to name the activities in the data set
activity_labels = read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("activity_id", "activity_label"),) #
dataset3 = merge(dataset2, activity_labels)

# Labeling the data set with descriptive activity names
selected_features$feature_label = gsub("\\(\\)", "", selected_features$feature_label)
selected_features$feature_label = gsub("-", ".", selected_features$feature_label)
for (i in 1:length(selected_features$feature_label)) {
    colnames(dataset3)[i + 3] <- selected_features$feature_label[i]
}
dataset4 = dataset3

## Creating a second, independent tidy data set with the average of each 
## variable for each activity and each subject
drops <- c("ID","activity_label")
dataset5 <- dataset4[,!(names(data4) %in% drops)]
aggdata <-aggregate(data5, by=list(subject = data5$subject_id, activity = dataset5$activity_id), FUN=mean, na.rm=TRUE)
drops <- c("subject","activity")
aggdata <- aggdata[,!(names(aggdata) %in% drops)]
aggdata = merge(aggdata, activity_labels)
write.csv(file="submit.csv", x=aggdata)
