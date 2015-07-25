# This script was developed and executed on an iMac running OSX Yosemite.
#
# Required packages:
#       1) dplyr
#       2) plyr
# 
# Make sure libraries are loaded and available for data frame manipulation.
library(dplyr)
library(plyr)
#
# Download the data set from the Source data URL to the R working directory
fileURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- "UCI_HAR_Dataset.zip"
download.file(fileURL, destfile = zipfile, method = "curl")

# The following commands were run manually and are not run as part of the script.
# These commands were used to interrogate the zip file structure so the individual
# file contents could be extracted. They are included here for information purposes only.
#       List files in zip archive:
#               > filelist <- unzip(zipfile, files=NULL, list=TRUE)
#               > filelist

# Extract the individual components of the raw data set.
activity_labels <- read.table(unz(zipfile, "UCI HAR Dataset/activity_labels.txt"), as.is = TRUE)
raw_var_list <- read.table(unz(zipfile, "UCI HAR Dataset/features.txt"), as.is = TRUE)

# Read the test set data.
subject_test <- read.table(unz(zipfile, "UCI HAR Dataset/test/subject_test.txt"), as.is = TRUE)
X_test <- read.table(unz(zipfile, "UCI HAR Dataset/test/X_test.txt"), as.is = TRUE)
y_test <- read.table(unz(zipfile, "UCI HAR Dataset/test/y_test.txt"), as.is = TRUE)

# Read the training set data.
subject_train <- read.table(unz(zipfile, "UCI HAR Dataset/train/subject_train.txt"), as.is = TRUE)
X_train <- read.table(unz(zipfile, "UCI HAR Dataset/train/X_train.txt"), as.is = TRUE)
y_train <- read.table(unz(zipfile, "UCI HAR Dataset/train/y_train.txt"), as.is = TRUE)

# Figuring out the data components
# The following results were found by using the str(), length(), and unique() functions on the 
# various data frames. This allows us to determine how the data set should be "re-assembled".
#
# Test Set analysis
# subject_test => 2947 obs. of 1 variable => 9 unique values of the variable => these are the test set volunteers
# X_test => 2947 obs. of 561 variables => these are the test set measures
# y_test => 2947 obs. of 1 variable => 6 unique values of the variable => activities

# Training Set analysis
# subject_train => 7352 obs. of 1 variable => 21 unique values of the variable => training set volunteers
# X_train => 7352 obs. of 561 variables => training set measures
# y_train => 7352 obs. of 1 variable => 6 unique values of the variable => activities

# Rename the columns in the subject_test and subject_train datasets.
names(subject_test) <- "subject"
names(subject_train) <- "subject"

# Match the activity codes with the activity descriptions and save in new data frames y_test1 and y_train1, 
# then rename the V2 column to "activity".
y_test1 <- join(y_test, activity_labels, by = "V1", type = "left") %>% select (V2)
names(y_test1) <- "activity"

y_train1 <- join(y_train, activity_labels, by = "V1", type = "left") %>% select (V2)
names(y_train1) <- "activity"

# Determining which variables to keep 
# The following two commands were executed manually and the results inspected
# to affirm their inclusion in the eventual tidy dataset.
#       > grep("mean", raw_var_list$V2, value = TRUE, ignore.case = FALSE)
#       > grep("std", raw_var_list$V2, value = TRUE, ignore.case = FALSE)
#
#       Note that "ignore.case=FALSE" excludes the angle() variables even though they contain the term "mean".
#       This data set excludes the angle() variables because they are calculating the angular velocity between two vectors
#       and are not mean or standard deviation calculations themselves.
#
#       If one wants to include the angular variables, then set "ignore.case" to TRUE and they will be included.
#       
#       The meanFreq() measures are included because they are a weighted average of the frequency components.
#       This is an inclusive interpretation of the requirements to include mean() and std() calculations.

# Create index vectors of the combined column names desired from the measures data set.
# These are the indices of the variable names to keep from raw_var_List, not the actual names.
meanLabels <- grep("mean", raw_var_list$V2, value = FALSE, ignore.case = FALSE)
stdDevLabels <- grep("std", raw_var_list$V2, value = FALSE, ignore.case = FALSE)

# raw_var_list[meanLabels,] => yields a vector of the mean measure names for the tidy data set 
# raw_var_list[stdLabels,] => yields a vector of the standard deviation measure names for the tidy data set
# The resulting td_var_list data frame contains the ordered list of 
# tidy data set measure names (V2) along with the
# column index of their location in the X_ measure variable data sets (V1).
td_var_list <- rbind(raw_var_list[meanLabels,], raw_var_list[stdDevLabels,]) %>% arrange(V1)

# Rename the td_var_list columns.
names(td_var_list) <- c("idx", "originalMeasureName")

# Make a more user friendly measure name by eliminating non-alphanumeric characters, specifically parentheses and hyphens.
measureName <- gsub("[^[:alnum:]]", "", td_var_list$originalMeasureName)

# Add the new measure names to the td_var_list data frame.
td_var_list <- cbind(td_var_list, measureName)

# Get test measures of interest.
X_test1 <- X_test[td_var_list$idx]

# Get training measures of interest.
X_train1 <- X_train[td_var_list$idx]

# Rename the measure variables in the test and training data sets with the user friendly names. 
names(X_test1) <- td_var_list$measureName
names(X_train1) <- td_var_list$measureName

# By column binding the subject_, X_, and y_ data frames in the respective 
# test and training data sets, a single data frame can be produced for each. 
# Row binding these two resulting data frames results in a 
# single consolidated data frame.

# Add the subject and activity to the test and training measure data sets.
# The order will be subject, then activity followed by the measures.
X_test1 <- cbind(cbind(subject_test, y_test1), X_test1)
X_train1 <- cbind(cbind(subject_train, y_train1), X_train1)

# Combine the test and training datasets and sort the rows.
tidyData1 <- rbind(X_test1, X_train1) %>% arrange(subject, activity)

# Write the tidy data set of raw signal measures to a tab delimited text file.
write.table(tidyData1, file = "RawSignals.txt", row.names = FALSE, sep = "\t")

# Compute the mean of each measure variable by subject and activity.
tidyData2 <- ddply(tidyData1, .(subject, activity), numcolwise(mean))

# Write the aggregated set of means to the MeanOfSignals tab delimited text file.
write.table(tidyData2, file = "MeanOfSignals.txt", row.names = FALSE, sep = "\t")

# These two datasets can be read into a data frame using these two commands:
# tidydata1 <- read.table("RawSignals.txt", header = TRUE)
# tidydata2 <- read.table("MeanOfSignals.txt", header = TRUE)
