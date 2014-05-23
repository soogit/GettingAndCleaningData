# Read the necessary compoents of the training dataset into temporary dataframes
dataset_train_1<-read.table("train/X_train.txt")
dataset_train_2<-read.table("train/y_train.txt")
dataset_train_3<-read.table("train/subject_train.txt")

# Read the necessary compoents of the test dataset into temporary variables
dataset_test_1<-read.table("test/X_test.txt")
dataset_test_2<-read.table("test/Y_test.txt")
dataset_test_3<-read.table("test/subject_test.txt")

# Read the names of the coloumns into a feature dataset 
features<-read.table("features.txt")

# Assign the appropriate coloumn names to the temporary dataframes
names(dataset_train_1)<-features[,2]
names(dataset_train_2)<-"Activity"
names(dataset_train_3)<-"Subject"
names(dataset_test_1)<-features[,2]
names(dataset_test_2)<-"Activity"
names(dataset_test_3)<-"Subject"

#Create the test and train dataset by merging the temporary dataframes
merged_dataset_train<-cbind(dataset_train_1,dataset_train_2,dataset_train_3)
merged_dataset_test<-cbind(dataset_test_1,dataset_test_2,dataset_test_3)

#Combine the test and train dataframes to create a full dataframe
comb_train_test<-rbind(merged_dataset_train,merged_dataset_test)

# Filter the names of coloums corresponding to mean and Standard Deviation from the features dataframe
std_mean <- c(".*mean\\().*", ".*std\\().*")
unique_std_mean <- unique(grep(paste(std_mean, collapse= "|"), features$V2, value=TRUE))

# Apply the filter to the full dataframe to get only those coloumns that correspond to mean and standard deviation 
refined_dataset <- comb_train_test[, c(unique_std_mean,"Activity", "Subject")]
head(refined_dataset)

# Reads the activity_labels
activity_labels <- read.table("activity_labels.txt")
names(activity_labels)<- c("Activity", "ActivityDescription")


# Merges the activity_labels with the refined dataset to add a coloumn which describes the activity
refined_dataset_desc <- merge(refined_dataset,activity_labels, all=TRUE)


# Loads the reshape2 package.
library(reshape2)

# Melts the refined data frame into ID and measure variables
melt_refined <- melt(refined_dataset_desc, id.vars=c("ActivityDescription","Subject","Activity"))

# Casts the melt and calulates the average of each measure variable for each activity and subject 
cast_refined <-  dcast(melt_refined, Subject + Activity + ActivityDescription  ~ variable, mean)

# The Activity coulumn is removed as it is unnecessary
fully_cleaned_data<-cast_refined[,-2]



# Modifies the coloumn names by eliminating intermittent hyphen and paranthesis symbols
names(fully_cleaned_data)<-gsub("-","",names(fully_cleaned_data))
names(fully_cleaned_data)<-gsub("\\()","",names(fully_cleaned_data))

# Writes the cleaned data to a text file in a tab separated format
write.table(fully_cleaned_data, "fully_cleaned_data.txt", sep="\t", row.names=FALSE)