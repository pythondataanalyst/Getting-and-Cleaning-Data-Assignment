library(dplyr)

#read actitvity labels
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt" ,col.names = c("activityId" ,"activityType"))

#read data description 
variable_names <- read.table("UCI HAR Dataset/features.txt")

#read train data
x_train <- read.table("UCI HAR Dataset/train/X_train.txt" ,col.names = variable_names[,2])
y_train <- read.table("UCI HAR Dataset/train/y_train.txt" ,col.names = "activityId")
sub_train <- read.table("UCI HAR Dataset/train/subject_train.txt" ,col.names = "subjectId")

#read test data
x_test <- read.table("UCI HAR Dataset/test/X_test.txt" , col.names = variable_names[,2])
y_test <- read.table("UCI HAR Dataset/test/y_test.txt" ,col.names = "activityId")
sub_test <- read.table("UCI HAR Dataset/test/subject_test.txt" ,col.names = "subjectId")

#1-Merge training and test sets to create one data set
x_total <- rbind(x_train ,x_test)
y_total <- rbind(y_train , y_test)
sub_total <- rbind(sub_train ,sub_test )
 
merg_data <- cbind(sub_total,x_total ,y_total)

#2-Extract only the mean and standard deviation measurements
colNames <- colnames(merg_data)
mean_std <- (grepl("activityId" , colNames) | grepl("subjectId" ,colNames) |grepl("mean.." ,colNames)
             |grepl("std.." ,colNames))

mean_and_std <- merg_data[, mean_std == TRUE]

#3-Uses descriptive activity names to name the activities in the data set
tidy_data <- merge(mean_and_std ,activity_labels ,by ="activityId" , all.x = TRUE)

#4-Appropriately labels the data set with descriptive variable names.
#I did that before 
        
#5-From the data set in step 4, creates a second,
#independent tidy data set with the average of each variable for each activity and each subject.
        
total_mean <- tidy_data %>%
        group_by(activityType ,subjectId)%>%
        summarise_each(funs =mean)

#export tidydata set
write.table(total_mean , "./finaldata.txt" ,sep = "\t" ,row.names = FALSE)

