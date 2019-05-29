#Merges the training and the test sets to create one data set.

library(plyr)

message("reading activity labels and features...")
activity_labels <- read.table("activity_labels.txt", quote="\"")
features <- read.table("features.txt", quote="\"")

message("reading test data...")
subject_test <- read.table("./test/subject_test.txt", quote="\"")
x_test <- read.table("./test/X_test.txt", quote="\"")
y_test <- read.table("./test/y_test.txt", quote="\"")

message("reading train data...")
subject_train <- read.table("./train/subject_train.txt", quote="\"")
x_train <- read.table("./train/X_train.txt", quote="\"")
y_train <- read.table("./train/y_train.txt", quote="\"")

message("merging test and train data...")
subject_data <- rbind(subject_test, subject_train)
x_data <- rbind(x_test, x_train)
y_data <- rbind(y_test, y_train)

message("naming columns...")
colnames(subject_data) <- "id"
colnames(y_data) <- "activity"
colnames(x_data) <- features$V2

#Extracts only the measurements on the mean and standard deviation for each measurement.

message("extracting mean and std measurements...")
x_data <- x_data[,grepl("mean\\(\\)", features$V2) | grepl("std\\(\\)", features$V2)]

data <- cbind(subject_data, y_data, x_data)

#Uses descriptive activity names to name the activities in the data set and Appropriately labels the data set with descriptive variable names.
message("renaming columns...")
new_names <- gsub("\\(\\)", "", names(data))
new_names <- gsub("^t", "time", new_names)
new_names <- gsub("^f", "frequency", new_names)
new_names <- gsub("\\-std", "Std", new_names)
new_names <- gsub("\\-mean", "Mean", new_names)
new_names <- gsub("\\-", "", new_names)
colnames(data) <- new_names

message("creating activity factor...")
data$activity <- factor(data$activity, labels=tolower(activity_labels$V2))

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
newData <- ddply(data, .(id, activity), numcolwise(mean))
write.table(newData, file="./tidydata.txt")

