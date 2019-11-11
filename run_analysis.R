library(tidyverse)

##You should be in the directory where you stored the dataset
x_test <- read.delim("./UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "", dec = ".")
y_test <- read.delim("./UCI HAR Dataset/test/Y_test.txt", col.names = c("Activity"),  header = FALSE, sep = "")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = c("Subject"), header = FALSE, sep = "")
features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE, sep = "", colClasses = "character")

x_train <- read.delim("./UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "", dec = ".")
y_train <- read.delim("./UCI HAR Dataset/train/Y_train.txt", col.names = c("Activity"),  header = FALSE, sep = "")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = c("Subject"), header = FALSE, sep = "")

activity_labels <- read.delim("./UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "")

#Add features as column names
colnames(x_test) <- unlist(features[2])
colnames(x_train) <- unlist(features[2])

#Add columns for the Activities and Subjects
x_test <- cbind(x_test, y_test, subject_test)
x_train <- cbind(x_train, y_train, subject_train)

#Merge training and test set
data <- rbind(x_test, x_train)

#Selecting mean and avg variables
#Dyplr solution returned duplicate column error so using base r
filtered_data <- data[grepl("mean|std", names(data), ignore.case = T)]

#Due to duplicate variable names I had to add the Acivity and Subject variables after selecting the other
#variables of interest
filtered_data <- cbind(filtered_data, Activity=data$Activity, Subject=data$Subject)

#Replace variables and Acvtivity observations with more descriptive values
clean_data <- filtered_data %>% 
  mutate(Activity=sapply(filtered_data$Activity, function(x){ x <- activity_labels[x, 2] }))

names(clean_data) <- gsub("^t", "time", names(clean_data))
names(clean_data) <- gsub("Acc", "Acceleration", names(clean_data))
names(clean_data) <- gsub("^f", "freq", names(clean_data))
names(clean_data) <- gsub("BodyBody", "Body", names(clean_data))


#Tidying up data
melted_dataset <- clean_data %>%
  pivot_longer(-(Activity:Subject), names_to = "Measurements", values_to = "Values")

tidy_dataset <- melted_dataset %>%  
  select(Subject, Activity, Measurements, Values) %>%
  arrange(Subject, Activity) %>%
  group_by(Subject, Activity, Measurements) %>%
  summarize(Avg=mean(Values))


