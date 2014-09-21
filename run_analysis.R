run_analysis <- function() {
  
  ######################################################################
  # Step 1 - Read "Test" and "Train" data sets and join the two data sets.
  #          This involves following for each data set :
  #          - read X, Y, and subject data into separate data frames
  #          - join these three data frames using cbind to form a coplete data set 
  #
  #          Finally, Use rbind to join train and test data sets
  ######################################################################
  
  # Read Test data from files
  x_test <- read.table("test/X_test.txt")
  y_test <- read.table("test/Y_test.txt") 
  subject_test <- read.table("test/subject_test.txt")

  # create Test data set
  test_data <- cbind(x_test, subject_test, y_test)
  
  # Read Training data from files
  x_train <- read.table("train/X_train.txt")
  y_train <- read.table("train/Y_train.txt") 
  subject_train <- read.table("train/subject_train.txt")
  
  # create train data set
  train_data <- cbind(x_train, subject_train, y_train)

  #merge Training and Test data sets into one data set
  merged_data <- rbind(train_data, test_data)
  
  ###################################################
  ## Step 1 Completed - merged_data now has training 
  #         and test data sets combined
  ###################################################
  
  ###########################################################
  # Step 2 - Extract Columns representing Mean and standard 
  # deviation for each of the measurements
  ###########################################################

  mean_std_data <- merged_data[c(1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215)]
  mean_std_data <- cbind(mean_std_data, merged_data[c(227:228, 240:241, 253:254, 266:271, 294:296, 345:350, 424:429, 452:454)])  
  mean_std_data <- cbind(mean_std_data, merged_data[c(503:504, 513, 516:517, 526,529, 539, 542:543, 552, 555:563)])
  
  #############################################################
  # step2 completed - mean_std_data data frame now contains only mean 
  # and standard devitation columns related to each measurement.
  #############################################################
  
  ###########################################################
  # Step 3 - Replace Activity Type vlaues with Activity labels
  #
  ###########################################################
 
  # Read Activity Labels file so we can replace the activity_id by the respective labels
  
  activity_labels <- read.table("activity_labels.txt")
  colnames(activity_labels) <- c("activity_id", "activity_name")
  
  # setting column names for activity_id and subject_id in mean_std_data set 
  colnames(mean_std_data)[ncol(mean_std_data)] <- "activity_id"
  colnames(mean_std_data)[ncol(mean_std_data)-1] <- "subject_id" 
  
  # Merge with mean_std_data and activity_labels data frames to get activity labels as a new column 
  mean_std_data <- merge(mean_std_data, activity_labels, by="activity_id")
   
  #Drop activity_id column from Merged Data Set so we are left with only activity_name
  mean_std_data$activity_id <- NULL 
 
  ###################################################
  ## Step 3 Completed
  ###################################################
  
 ###############################################################
 ##  Step 4 - Label the data set with descriptive column names
 ###############################################################
 
 colnames(mean_std_data)[1:3] <- c("tBodyAcc-mean-X", "tBodyAcc-mean-Y", "tBodyAcc-mean-Z")
 colnames(mean_std_data)[4:6] <- c("tBodyAcc-std-X", "tBodyAcc-std-Y", "tBodyAcc-std-Z")
 
 colnames(mean_std_data)[7:9] <- c("tGravityAcc-mean-X", "tGravityAcc-mean-Y", "tGravityAcc-mean-Z")
 colnames(mean_std_data)[10:12] <- c("tGravityAcc-std-X", "tGravityAcc-std-Y", "tGravityAcc-std-Z")

 colnames(mean_std_data)[13:15] <- c("tBodyAccJerk-mean-X", "tBodyAccJerk-mean-Y", "tBodyAccJerk-mean-Z")
 colnames(mean_std_data)[16:18] <- c("tBodyAccJerk-std-X", "tBodyAccJerk-std-Y", "tBodyAccJerk-std-Z")
 
 colnames(mean_std_data)[19:21] <- c("tBodyGyro-mean-X", "tBodyGyro-mean-Y", "tBodyGyro-mean-Z")
 colnames(mean_std_data)[22:24] <- c("tBodyGyro-std-X", "tBodyGyro-std-Y", "tBodyGyro-std-Z")
 
 colnames(mean_std_data)[25:27] <- c("tBodyGyroJerk-mean-X", "tBodyGyroJerk-mean-Y", "tBodyGyroJerk-mean-Z")
 colnames(mean_std_data)[28:30] <- c("tBodyGyroJerk-std-X", "tBodyGyroJerk-std-Y", "tBodyGyroJerk-std-Z")
 
 colnames(mean_std_data)[31:34] <- c("tBodyAccMag-mean", "tBodyAccMag-std", "tGravityAccMag-mean", "tGravityAccMag-std")
 colnames(mean_std_data)[35:38] <- c("tBodyAccJerkMag-mean", "tBodyAccJerkMag-std", "tBodyGyroMag-mean", "tBodyGyroMag-std")
 colnames(mean_std_data)[39:40] <- c("tBodyGyroJerkMag-mean", "tBodyGyroJerkMag-std")

 colnames(mean_std_data)[41:43] <- c("fBodyAcc-mean-X", "fBodyAcc-mean-Y", "fBodyAcc-mean-Z")
 colnames(mean_std_data)[44:46] <- c("fBodyAcc-std-X", "fBodyAcc-std-Y", "fBodyAcc-std-Z")
 colnames(mean_std_data)[47:49] <- c("fBodyAcc-meanFreq-X", "fBodyAcc-meanFreq-Y", "fBodyAcc-meanFreq-Z")

 colnames(mean_std_data)[50:52] <- c("fBodyAccJerk-mean-X", "fBodyAccJerk-mean-Y", "fBodyAccJerk-mean-Z")
 colnames(mean_std_data)[53:55] <- c("fBodyAccJerk-std-X", "fBodyAccJerk-std-Y", "fBodyAccJerk-std-Z")
 
 colnames(mean_std_data)[56:58] <- c("fBodyGyro-mean-X", "fBodyGyro-mean-Y", "fBodyGyro-mean-Z")
 colnames(mean_std_data)[59:61] <- c("fBodyGyro-std-X", "fBodyGyro-std-Y", "fBodyGyro-std-Z")
 colnames(mean_std_data)[62:64] <- c("fBodyGyro-meanFreq-X", "fBodyGyro-meanFreq-Y", "fBodyGyro-meanFreq-Z")
 
 colnames(mean_std_data)[65:67] <- c("fBodyAccMag-mean", "fBodyAccMag-std", "fBodyAccMag-meanFreq")
 
 colnames(mean_std_data)[68:70] <- c("fBodyBodyAccJerkMag-mean", "fBodyBodyAccJerkMag-std", "fBodyBodyAccJerkMag-meanFreq")
 
 colnames(mean_std_data)[71:72] <- c("fBodyBodyGyroMag-mean", "fBodyBodyGyroMag-meanFreq")
 
 colnames(mean_std_data)[73:75] <- c("fBodyBodyGyroJerkMag-mean", "fBodyBodyGyroJerkMag-std", "fBodyBodyGyroJerkMag-meanFreq")
 
 colnames(mean_std_data)[76:82] <- c("angle1", "angle2", "angle3", "angle4", "angle5", "angle6", "angle7")
 
 ###################################################
 ## Step 4 Completed
 ###################################################
 
 ###############################################################
 ##  Step 5 - Create a tidy data set
 ###############################################################
 
 ############
 # Using aggregate() function to calculate mean for each measurement grouped by subject and activity
 ##############
 tidy_dataset <- aggregate(mean_std_data, by=list(mean_std_data$activity_name, mean_std_data$subject_id), FUN=mean, na.rm=TRUE)
 
 #order the tidy data set by activity and subject 
 tidy_dataset <- tidy_dataset[order(tidy_dataset$Group.1, tidy_dataset$Group.2), ]
 
 # drop the subject_id and activity_name columns and rename Group1 & 2 columns appropriately
 tidy_dataset$subject_id <- NULL
 tidy_dataset$activity_name <- NULL
 colnames(tidy_dataset)[1:2] <- c("activity_name", "subject_id")
 
 #########################################
 # write the final data set to a text file
 #########################################
 write.table(tidy_dataset, file = "project_output.txt",row.names=FALSE, na="")
 
 ## End of the Progra. outout is in the file named project_output.txt !
 
}