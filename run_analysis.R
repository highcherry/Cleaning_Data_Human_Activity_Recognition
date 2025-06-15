
library(dplyr)
library(tidyr)

## Step 0: Load all data files ------------------------------------------------
# Read feature names and activity labels
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("id", "feature"))
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activity_id", "activity"))

# Read and make training dataset
train_sub <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
train_act <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activity_id")
train_measurements <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$feature)
train_set <- cbind(train_sub, train_act, train_measurements)


# Read and make test dataset 
test_sub <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
test_act <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activity_id")
test_measurements <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$feature)
test_set <- cbind(test_sub, test_act, test_measurements)  

## Step 1: Merges the training and the test sets to create one data set.
combined_data <- rbind(train_set,  test_set )

## Step 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
# Select only mean/std columns
filtered_data <- combined_data %>% 
  select(subject, activity_id, colnames(combined_data )[grep("mean\\.\\.|std\\.\\.", names(combined_data))])

## Step 3: Uses descriptive activity names to name the activities in the data set

labeled_data <- filtered_data %>%
  left_join(activity_labels, by = "activity_id") %>%
  #select(-activity_id) %>%               # Remove numeric activity code
  relocate(subject,activity_id, activity)            # Move identifier columns to front

names(labeled_data)  <- gsub("\\.\\.\\.", "_", names(labeled_data)  )  %>%  tolower()  # or str_replace(names(labeled_data) ,"\\.\\.\\.", "_")


## Step 4: Label with descriptive variable names -----------------------------
# Clean up feature names
clean_feature_names <- features$feature[mean_std_cols] %>%
  gsub("[()]", "", .) %>%               # Remove parentheses
  gsub("-", "_", .) %>%                 # Replace hyphens with underscores
  tolower()                             # Convert to lowercase

## Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_summary <- labeled_data %>%
    select(-activity_id) %>% 
    group_by(subject, activity) %>%
    summarise(across(everything(), mean))

write.table(tidy_summary, "tidy_summary.txt", row.names = FALSE)
