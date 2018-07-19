library(dplyr)

##download data from the website to Data Science directory and unzip the data
setwd(“./Desktop/coursera/Data Science”)
if(!file.exists(“./Cleaning Data“)) dir.create(“./Cleaning Data”)
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(Url, destfile = "./data/projectData_getCleanData.zip")
Upzip_data <- unzip("./data/projectData_getCleanData.zip", exdir = "./data")

##preparing the testing data
#read the data
X_test <- read.table("./Cleaning Data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./Cleaning Data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./Cleaning Data/UCI HAR Dataset/test/subject_test.txt")
#create a variable noting the data as "test" (not necessary)
Test_Data <- mutate(X_test, note = “test”)
#combine testing data with subject and activity variable
Test_Data <- cbind(X_test, y_test, subject_test)

##preparing the training data
#read the data
X_train <- read.table("./Cleaning Data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./Cleaning Data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./Cleaning Data/UCI HAR Dataset/train/subject_train.txt")
#create a variable noting the data as "train" (not necessary)
Train_Data <- mutate(X_train, labels = y_train[[1]], note = "train")
#combine training data with subject and activity variable
Train_Data <- cbind(X_train, y_train, subject_train)

##merge the test and train dataset
Data_merge <- rbind(Test_Data, Train_Data)


##make the variable names and activity descriptive and select those "mean" and "std" measurement
#read the variable names file
variables <- read.table("./Cleaning Data/UCI HAR Dataset/features.txt")
variables <- variables[[2]]
variables <- as.character(variables)
#select from the variable file the index of the "mean" and "std" measurement
condition <- grep("mean\\(\\)|std\\(\\)",variables)
#select from the variable file the actual names of the selected variables
#this file should match to the selected data
variables_condition = variables[condition]

#select from the merged data the "mean" and "std" data in addition to "note","activity" and "subject"
#this file should match to the selected variable names
Data_select <- Data_merge[,c(condition,562:564)]
#rename the variable names 
colnames(Data_merge_select) = c(variables_condition,”note”,”activity”,”subject”)

#revise the variable names to make it more readable
names(Data_select) <- gsub("^t", "Time ", names(Data_select)) 
names(Data_select) <- gsub("^f", "Frequence ", names(Data_select))
names(Data_select) <- gsub("-mean\\(\\)", " Mean ", names(Data_select))
names(Data_select) <- gsub("-std\\(\\)", " Standard Deviation ", names(Data_select))

#make the activity names descriptive
#select the activity file
activity <- read.table("./Cleaning Data/UCI HAR Dataset/activity_labels.txt")
#revise the activity names
Data_select$activity <- factor(Data_select$activity, levels = activity[,1], labels = activity[,2])

#exculde the note variable
Data_2.0 <- select(Data_select,-note)
##this Data_2.0 file is ready for grouping and summarizing to approach the final data

##creates a second, independent tidy data set with the average of each variable for 
##each activity and each subject
#regroup data_2.0
Data_group = group_by(Data_2.0,activity,subject)
#summarize data _2.0
Data_summary = summarize_if(Data_group,is.numeric, mean, na.rm = TRUE)

#save the final dataset
Data_3.0 = Data_final
write.table(Data_3.0,"./Cleaning Data/UCI HAR Dataset/Data_3.0.txt")
            