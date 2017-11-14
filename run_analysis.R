#1. Merges the training and the test sets to create one data set.
#read in test file and training file
setwd('/Users/boyaliu/Documents/Coursera/Data Science/Cleaning Data/Project/UCI HAR Dataset')
#library(dplyr)

# read training data
x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_train <- read.table('train/subject_train.txt')

# read test data
x_test <- read.table('test/X_test.txt')
y_test <- read.table('test/y_test.txt')
subject_test <- read.table('test/subject_test.txt')

# combine training data and test data
all_x <- rbind(x_train, x_test)
all_y <- rbind(y_train, y_test)
all_subject <- rbind(subject_train, subject_test)
all_df <- cbind(all_x, all_y, all_subject)

# change column names
var_name <- read.table('features.txt', stringsAsFactors = FALSE)
colnames(all_df) <- c(var_name$V2, "activity_labels", "subject")

#2. Extracts only the measurements on the mean and standard deviation for each measurement
mean_std_colnames <- grep("mean\\(\\)|std\\(\\)", colnames(all_df), ignore.case = T, value = T)
mean_std_colnames <- c(mean_std_colnames, 'activity_labels', 'subject')
# extract data with mean or std in column names
all_df <- all_df[, mean_std_colnames]

#3. Uses descriptive activity names to name the activities in the data set
activity_labels_df <- read.table('activity_labels.txt', colClasses = c('integer', 'factor'))
colnames(activity_labels_df) <- c("activity_num", 'activity_labels')
all_df$activity_labels <- as.factor(all_df$activity_labels)
all_df$activity_labels <- activity_labels_df$activity_labels[match(all_df$activity_labels, activity_labels_df$activity_num)]

#4. Appropriately labels the data set with descriptive variable names.
# chane t with time and f with frequency, Acc with Accelerometer,
# Gyro with Gyroscope, Mag with Magnitude in column names and remove () in column names
change_f_colname <- gsub("^f", "Frequency", names(all_df), ignore.case = T)
change_f_t_colname <- gsub("^t", "Time", change_f_colname, ignore.case = T)
change_colname <- gsub("Acc", "Accelerometer", change_f_t_colname)
change_colname <- gsub("Gyro", "Gyroscope", change_colname)
change_colname <- gsub("Mag", "Magnitude", change_colname)
change_colname <- gsub("\\(\\)", "", change_colname)
change_colname <- gsub("tBody", "TimeBody", change_colname)
colnames(all_df) <- change_colname

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
all_df$subject <- as.factor(all_df$subject)
summary_df <- aggregate(. ~ subject + activity_labels, all_df, mean)
write.table(summary_df, file = "tidy_df.txt", row.names = FALSE)
