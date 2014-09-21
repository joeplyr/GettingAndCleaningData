library(plyr)

download.data = function() {
    "Creates new data directory if one does not already exist"
    if (!file.exists("data")) {
        dir.create("data")
    }
    if (!file.exists("data/UCI HAR Dataset")) {
        # download the dataset
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        zipfile="data/UCI_HAR_data.zip"
        download.file(fileURL, destfile=zipfile, method="curl")
        unzip(zipfile, exdir="data")
    }
}

merge.datasets = function() {
    "Merge training and test datasets"
    # Read the data sets
    message("reading X_train.txt")
    training.x <- read.table("data/UCI HAR Dataset/train/X_train.txt")
    message("reading y_train.txt")
    training.y <- read.table("data/UCI HAR Dataset/train/y_train.txt")
    message("reading subject_train.txt")
    training.subject <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
    message("reading X_test.txt")
    test.x <- read.table("data/UCI HAR Dataset/test/X_test.txt")
    message("reading y_test.txt")
    test.y <- read.table("data/UCI HAR Dataset/test/y_test.txt")
    message("reading subject_test.txt")
    test.subject <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
    # Merge the data sets
    merged.x <- rbind(training.x, test.x)
    merged.y <- rbind(training.y, test.y)
    merged.subject <- rbind(training.subject, test.subject)
    # merge the train and the test data
    list(x=merged.x, y=merged.y, subject=merged.subject)
}

extract.mean.and.std = function(df) {
    # Extract only the measurements on the mean and standard deviation for each measurement. 
   

    # Read the features file
    
    features <- read.table("data/UCI HAR Dataset/features.txt")
    
    # Find the mean and std cols
    
    mean.col <- sapply(features[,2], function(x) grepl("mean()", x, fixed=T))
    std.col <- sapply(features[,2], function(x) grepl("std()", x, fixed=T))
    # Extract mean and std cols from the data set
    
    edf <- df[, (mean.col | std.col)]
    colnames(edf) <- features[(mean.col | std.col), 2]
    edf
}

name.activities = function(df) {

    # Utilize the provided descriptive activity names
    
    
    colnames(df) <- "activity"
    df$activity[df$activity == 1] = "WALKING"
    df$activity[df$activity == 2] = "WALKING_UPSTAIRS"
    df$activity[df$activity == 3] = "WALKING_DOWNSTAIRS"
    df$activity[df$activity == 4] = "SITTING"
    df$activity[df$activity == 5] = "STANDING"
    df$activity[df$activity == 6] = "LAYING"
    df
}

bind.data <- function(x, y, subjects) {
    # Combine the mean and std values (x), activities (y) and subjects into one data set.
    
    cbind(x, y, subjects)
}

create.tidy.dataset = function(df) {
    # Create an independent tidy dataset with avg of each subject/activity.
   
    tidy <- ddply(df, .(subject, activity), function(x) colMeans(x[,1:60]))
    tidy
}

clean.data = function() {
    # Download the data
    download.data()
    
    # merge training and test datasets.
    
    merged <- merge.datasets()
    # Extract only the measurements of the mean and standard deviation for each measurement
  
    cx <- extract.mean.and.std(merged$x)
    # Name activities
    cy <- name.activities(merged$y)
    # Use descriptive column name for subjects
    colnames(merged$subject) <- c("subject")
    # Combine data frames into one
    combined <- bind.data(cx, cy, merged$subject)
    # Create tidy dataset
    tidy <- create.tidy.dataset(combined)
    # Write tidy dataset as csv
    write.csv(tidy, "UCI_HAR_tidy.csv", row.names=FALSE)
}
