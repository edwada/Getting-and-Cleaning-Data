library(plyr)
library(dplyr)

download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
              destfile = "Data//ProjectData.zip")

unzip(zipfile = "Data//ProjectData.zip", overwrite = TRUE, exdir = "Data//ProjectData")


#loading test data
subjects_test <- read.table("Data\\ProjectData\\UCI HAR Dataset\\test\\subject_test.txt")
X_test <- read.table("Data\\ProjectData\\UCI HAR Dataset\\test\\X_test.txt")
Y_test <- read.table("Data\\ProjectData\\UCI HAR Dataset\\test\\Y_test.txt")

subjects_train <- read.table("Data\\ProjectData\\UCI HAR Dataset\\train\\subject_train.txt")
X_train <- read.table("Data\\ProjectData\\UCI HAR Dataset\\train\\X_train.txt")
Y_train <- read.table("Data\\ProjectData\\UCI HAR Dataset\\train\\Y_train.txt")

subjects_merged <- rbind(subjects_test, subjects_train)
X_merged <- rbind(X_test, X_train) 
Y_merged <- rbind(Y_test, Y_train) 


features <- read.table("Data\\ProjectData\\UCI HAR Dataset\\features.txt")
activity_labels <- read.table("Data\\ProjectData\\UCI HAR Dataset\\activity_labels.txt")
activity_factor <- factor(activity_labels[,2])

subjects_merged <- rename(subjects_merged, c("V1" = "Subject"))
colnames(X_merged) <- features[,2]
colnames(Y_merged) <- c("Activity")
Y_merged <- mutate(Y_merged, Activity = activity_factor[Activity])

names(X_merged) <- make.names(names=names(X_merged), unique=TRUE, allow_ = TRUE)
X_merged[, -grep("mean\\(\\)|std\\(\\)", colnames(df))]




tidyData <- cbind(subjects_merged, Y_merged, X_merged)

result <- group_by(tidyData, Subject, Activity) %>% summarise_each(funs(mean))

write.table(x = result, file = "result.txt", row.name = FALSE)
