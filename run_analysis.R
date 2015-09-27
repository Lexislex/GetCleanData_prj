run_analysis <- function(){
    #load library for casting final data frame
    library(reshape2)
    #creating necessary variables
    #tidy_data <- data.frame()
    cursor <- c()
    columns <- c()
    
    #creating features table
    features <- read.table("features.txt", 
                           col.names = c("id", "var_name"), 
                           stringsAsFactors = F)
    #add variable for select options for extracting data
    features$extract = grepl("mean()", features$var_name) | grepl("std()", features$var_name)
    #creating activity label table for naming rows
    activity_labels <- read.table("activity_labels.txt",
                                  col.names = c("id", "activity_lbl"),
                                  stringsAsFactors = F)
    #creating and merging tables with test and train activities information
    activity <- read.table("./test/y_test.txt",
                           col.names = c("id"))
    activity <- rbind(activity, read.table("./train/y_train.txt",
                                           col.names = c("id")))
    #merging activity data with labels
    activity <- merge(activity, activity_labels)
    #creating and merging tables with test and train subjects information
    subject <- read.table("./test/subject_test.txt",
                          col.names = c("id"))
    subject <- rbind(subject, read.table("./train/subject_train.txt",
                                col.names = c("id")))
    #creating vector winth necessary descriptive columns name and vector with width of fields
    #for extracting "mean()" and "std()" data
    for(i in features$id){
        if(features$extract[i]){
            cursor <- c(cursor, 16)
            columns <- c(columns, features$var_name[i])
            }else{
            cursor <- c(cursor, -16)
            }
    }
    #extracting and merging only necessary data
    tidy_data <- read.fwf("./test/x_test.txt", 
                          widths = cursor, 
                          col.names = columns)
    tidy_data<- rbind(tidy_data, read.fwf("./train/x_train.txt", 
                                          widths = cursor, 
                                          col.names = columns))
    #adding information about subjects and its activities to dataset
    tidy_data$activity_l <- activity[,2]
    tidy_data$subject_id <- subject[,1]
    #melting and casting dataset
    tidy_data <- melt(tidy_data, id = c("activity_l", "subject_id"), measure.vars = names(tidy_data[,1:79]))
    tidy_data <- dcast(tidy_data, subject_id + activity_l ~ variable,mean)
    tidy_data
}
