# Getting and Cleaning Data Assignment by Sunil Buge Aug 20 2014
# Downloaded and extracted raw data set under folder UCI HAR Dataset
# R program written --- run_analysis.R
# direcotry under working location by name ---UCI HAR Dataset
# Step 1 write merge_data function having parameter directory name 
# read various files by read.table function to create sinle data set

merge_data_sets <- function(directory) {
        # read the raw dataset in data frame test_data and train_data
        path <- paste("./", directory, "/test/X_test.txt", sep="")
        test_data <- read.table(path)
        path <- paste("./", directory, "/train/X_train.txt", sep="")
        train_data <- read.table(path)
        # read the activity labels text file 
        path <- paste("./", directory, "/activity_labels.txt", sep="")
        activity_labels <- read.table(path)
        # read the subject labels for test and train data sets 
        path <- paste("./", directory, "/train/subject_train.txt", sep="")
        subject_train <- read.table(path)
        path <- paste("./", directory, "/test/subject_test.txt", sep="")
        subject_test <- read.table(path)
        # read the lables for y test and train data sets 
        path <- paste("./", directory, "/train/y_train.txt", sep="")
        y_train <- read.table(path)
        path <- paste("./", directory, "/test/y_test.txt", sep="")
        y_test <- read.table(path)
        # merge labels for y test and train activity 
        y_train_labels <- merge(y_train,activity_labels,by="V1")
        y_test_labels <- merge(y_test,activity_labels,by="V1")
        # merge the all lables and data together for test and train 
        train_data <- cbind(subject_train,y_train_labels,train_data)
        test_data <- cbind(subject_test,y_test_labels,test_data)
        # use rowbind function to merge test and train data 
        all_data_set <- rbind(train_data,test_data)
        # return merged data 
        return (all_data_set)
}

# setp 2 - Extracts only the measurements on the mean and standard deviation for each measurement
# step 3 - Uses descriptive activity names to name the activities in the data set
# step 4 - Appropriately labels the data set with descriptive activity names
# Written function extract_mean_std(data_set, directory) having 2 parameters
# one data set and location directory
extract_mean_std <- function(data_set, directory) {
        # Read features data into data frame 
        path <- paste("./", directory, "/features.txt", sep="")
        features_data <- read.table(path)
        # found 2 columns - V1 like column number
        # and V2 like column actual name
        # subseting only rows that contains the word mean and std
        mean_std_rows <- subset(features_data, grepl("(mean\\(\\)|std\\(\\))", features_data$V2) )
        # assign column heading to data set like Subject, activity_id, activity
        colnames(data_set) <- c("Subject","Activity_Id","Activity",as.vector(features_data[,2]))
        # extracting the data from the merged data set for column names like mean OR std
        mean_columns <- grep("mean()", colnames(data_set), fixed=TRUE)
        std_columns <- grep("std()", colnames(data_set), fixed=TRUE)
        # combine both columns mean and std into vector
        mean_std_column_vector <- c(mean_columns, std_columns)
        # sorting the vector using sort function
        mean_std_column_vector <- sort(mean_std_column_vector)
        # extracting columns with std and mean 
        extracted_data_set <- data_set[,c(1,2,3,mean_std_column_vector)]
        return (extracted_data_set)
}

# Step 5 Creates a second, independent tidy data set with the average of each variable for each activity 
# and each subject.
# This is last setp in assignment 
# Written functions melt_data and write_tidy_set into file
# used extracted_data_set as input parameter
# path where to write output tidy data set txt file
melt_data_and_create_tidy_set <- function(data_set, path_to_tidyset_file) {
        # let's melt the data
        require(reshape2)
        melted_data <- melt(data_set, id=c("Subject","Activity_Id","Activity"))
        # casting the data set to the tidy_data format
        tidy_data <- dcast(melted_data, formula = Subject + Activity_Id + Activity ~ variable, mean)
        # let us format the column names
        col_names_vector <- colnames(tidy_data)
        # use gsub global substiute to clean column names
        col_names_vector <- gsub("-mean()","Mean",col_names_vector,fixed=TRUE)
        col_names_vector <- gsub("-std()","Std",col_names_vector,fixed=TRUE)
        col_names_vector <- gsub("BodyBody","Body",col_names_vector,fixed=TRUE)
        # assign clean column names back to tidy column names
        colnames(tidy_data) <- col_names_vector
        # write the output into a file
        write.table(tidy_data, file=path_to_tidyset_file, sep="\t", row.names=FALSE)
}
