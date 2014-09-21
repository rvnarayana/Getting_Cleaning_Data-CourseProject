##############################################################
# This is file explains the steps used in the course project for 
# Getting and Cleaning Data Course Project. 
##############################################################

Per the Course Project requirements the run_analysis.R file in this repo does the following :

- Reads test and train data sets from files 
- Extracts only the measurements on the mean and standard deviation for each measurement
- Assigns descriptive activity names to name the activities in the data set
- Appropriately labels the data set with descriptive variable names. 
- Creates a tidy data set with the average of each variable for each activity and each subject.
- writes the final outoput to the file project_output.txt

###########################################################################
##
## Below is a detailed description of how each of the steps was implemented
##
########################################################################### 


Step 1 - Read "Test" and "Train" data sets and join the two data sets
---------------------------------------------------------------------

This involves following for each data set :

	- Read X, Y, and subject data into separate data frames
        - Join these three data frames using cbind to form a coplete data set 

	Finally, Use rbind to join train and test data sets to create a complete data set
	called merged_data
  
 
Step 2 - Extract Columns representing Mean and standard deviation for each of the measurements
-------------------------------------------------------------------------------------

	- Extract appropriate columns based on column indices and create a new data frame 
	  called mean_std_data


Step 3 - Replace Activity Type vlaues with Activity labels
------------------------------------------------------------
  
	- Read Activity Labels file
        - merge with mean_std_data data frame
	- drop activity_id column

 
Step 4 - Replace Activity Type vlaues with Activity labels
------------------------------------------------------------
  
	- Assign self descriptive column names based on features description


Step 5 - Create a tidy data set and write it to a file
------------------------------------------------------------
 
	- Using aggregate() function to calculate mean for each measurement 
	  grouped by subject and activity

	- order the tidy data set by activity and subject 
	- drop the subject_id and activity_name columns and rename Group1 & 2 columns appropriately
	- write the data set to the file called project_output.txt using write,table() function
 
