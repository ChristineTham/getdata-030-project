# getdata-030: Getting and Cleaning Data
# Course Project
# Christine Tham
# 
# R script to collect, work with, and clean a data set
# Outputs a data file called "UCI_HAR_tidy.txt"
#
# Read metadata files
activity_labels <- read.table("./activity_labels.txt", sep = " ",
                              col.names = c("ID", "Activity"))
feature <- read.table("./features.txt", sep = " ",
                       col.names = c("ID","Feature"),
                       stringsAsFactors = FALSE)
# Creatise sanitised column names from feature names
feature$colname <- gsub("-", ".", feature$Feature, fixed = TRUE)
feature$colname <- gsub(",", "_", feature$colname, fixed = TRUE)
feature$colname <- gsub("()", "_value", feature$colname, fixed = TRUE)
feature$colname <- gsub("(", ".", feature$colname, fixed = TRUE)
feature$colname <- gsub(")", ".", feature$colname, fixed = TRUE)
# Read training set, using column names from feature
subject_train <- read.table("./train/subject_train.txt", sep = " ",
                            col.names = c("Subject"))
x_train <- read.fwf("./train/X_train.txt", widths = rep(16, 561),
                    col.names = feature$colname)
y_train <- read.table("./train/y_train.txt", sep = " ",
                      col.names = "Activity")
# Training intertial signals - not read as won't be used in tidy data set
# body_acc_x_train <- read.fwf("./train/Inertial Signals/body_acc_x_train.txt", widths = rep(16, 128))
# body_acc_y_train <- read.fwf("./train/Inertial Signals/body_acc_y_train.txt", widths = rep(16, 128))
# body_acc_z_train <- read.fwf("./train/Inertial Signals/body_acc_z_train.txt", widths = rep(16, 128))
# body_gyro_x_train <- read.fwf("./train/Inertial Signals/body_gyro_x_train.txt", widths = rep(16, 128))
# body_gyro_y_train <- read.fwf("./train/Inertial Signals/body_gyro_y_train.txt", widths = rep(16, 128))
# body_gyro_z_train <- read.fwf("./train/Inertial Signals/body_gyro_z_train.txt", widths = rep(16, 128))
# total_acc_x_train <- read.fwf("./train/Inertial Signals/total_acc_x_train.txt", widths = rep(16, 128))
# total_acc_y_train <- read.fwf("./train/Inertial Signals/total_acc_y_train.txt", widths = rep(16, 128))
# total_acc_z_train <- read.fwf("./train/Inertial Signals/total_acc_z_train.txt", widths = rep(16, 128))
# Read test set, using same apprach as training set
subject_test <- read.table("./test/subject_test.txt", sep = " ",
                            col.names = c("Subject"))
x_test <- read.fwf("./test/X_test.txt", widths = rep(16, 561),
                    col.names = feature$colname)
y_test <- read.table("./test/y_test.txt", sep = " ",
                      col.names = "Activity")
# Test Inertial Signals - ignored
# body_acc_x_test <- read.fwf("./test/Inertial Signals/body_acc_x_test.txt", widths = rep(16, 128))
# body_acc_y_test <- read.fwf("./test/Inertial Signals/body_acc_y_test.txt", widths = rep(16, 128))
# body_acc_z_test <- read.fwf("./test/Inertial Signals/body_acc_z_test.txt", widths = rep(16, 128))
# body_gyro_x_test <- read.fwf("./test/Inertial Signals/body_gyro_x_test.txt", widths = rep(16, 128))
# body_gyro_y_test <- read.fwf("./test/Inertial Signals/body_gyro_y_test.txt", widths = rep(16, 128))
# body_gyro_z_test <- read.fwf("./test/Inertial Signals/body_gyro_z_test.txt", widths = rep(16, 128))
# total_acc_x_test <- read.fwf("./test/Inertial Signals/total_acc_x_test.txt", widths = rep(16, 128))
# total_acc_y_test <- read.fwf("./test/Inertial Signals/total_acc_y_test.txt", widths = rep(16, 128))
# total_acc_z_test <- read.fwf("./test/Inertial Signals/total_acc_z_test.txt", widths = rep(16, 128))
# Name Activities in test and training
y_test$Activity_Label <- activity_labels[y_test$Activity,"Activity"]
y_train$Activity_Label <- activity_labels[y_train$Activity,"Activity"]
# Merges test and training data components into two data frames
test_df <- cbind(subject_test, y_test, x_test)
train_df <- cbind(subject_train, y_train, x_train)
# Merges training and test sets into single data set
data <- rbind(test_df, train_df)
# Do some simple tests to ensure data is clean - all tests should return 0
sum(is.na(data[,4:564]))
sum(data[,4:564] < -1)
sum(data[,4:564] > 1)
# View storage size
object.size(data)
# Use dplyr library to create tidy data set
library(dplyr)
data_tidy <- tbl_df(data) %>%
  select(Subject, Activity_Label, contains("mean_value"), contains("std_value")) %>%
  group_by(Subject, Activity_Label) %>%
  summarise_each(funs(mean))
write.table(data_tidy, "UCI_HAR_tidy.txt", row.names = FALSE)