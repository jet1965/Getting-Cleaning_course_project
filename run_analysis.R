run_analysis <- function(){
  # Get the zipped dataset, store it and and unzip it. Uncomment these two lines to download and unzip the dataset.
  # A copy of the data is already in the repo, making this step unnecessary.
  # download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "./data/DataSet.zip")
  # unzip(zipfile = "./data/DataSet.zip")
  
  # Create a dataframe for the training data set, one for the activity labels, and bind them into one dataframe.
  dsTrain_data <- fread("./UCI HAR Dataset/train/X_train.txt")
  dsTrain_Activity <- fread("./UCI HAR Dataset/train/y_train.txt")
  dsTrain_Subject <- fread("./UCI HAR Dataset/train/subject_train.txt")
  dsTrain_df <- cbind(dsTrain_Activity, dsTrain_Subject, dsTrain_data)
  
  #Repeat the dataframe creation and binding process for the testing dataset.
  dsTest_data <- fread("./UCI HAR Dataset/test/X_test.txt")
  dsTest_Activity <- fread("./UCI HAR Dataset/test/y_test.txt")
  dsTest_Subject <- fread("./UCI HAR Dataset/test/subject_test.txt")
  dsTest_df <- cbind(dsTest_Activity, dsTest_Subject, dsTest_data)
  
  # Create a dataframe of indexed variable names from the features text file.
  dfl <- fread("./UCI HAR Dataset/features.txt")
  # Use "Activity plus the variable names from dfl to create a 
  # character vector of labels for the training and testing dataframes.
  dfLabels <- append(c("Activity", "Subject"), dfl$V2)
  
  # Label the columns of the training and testing datasets, then bind them together into one dataframe.
  names(dsTest_df) <- dfLabels
  names(dsTrain_df) <- dfLabels
  ds_df <- rbind(dsTrain_df, dsTest_df)
  
  # Use "mean|std" to filter the columns of the master dataframe so only mean and std variables are kept.
  stat_re <- "mean|std|Activity|Subject"
  stat_names <- names(ds_df)[grepl(stat_re,names(ds_df))]
  ds_df_stat <- ds_df[ , stat_names, with = FALSE]

  #Create second, independent, tidy data set with the average of each variable 
  # for each activity and each subject.
  tidy_ds_df <- aggregate(x = ds_df_stat, by = list(ds_df_stat$Activity, ds_df_stat$Subject), FUN = mean)
  tidy_ds_df <- tidy_ds_df[ , -c(1:2)] # Remove redundant xcolumns created by aggregate function.
    
}



