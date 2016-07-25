## R getting and cleaning data Assignment

#You should create one R script called run_analysis.R that does the following.

#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#Good luck!

#------------------------------------------------------------------------------------------------------------------------------------------------

##The function below is used to install multiple packages at once


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


## going based off the train and test folders there will be much data manipulation
## The Data.table and reshape2 package will be useful in arranging the data and presenting it in a neat format

packages<- c("data.table","reshape2")

## This preliminary check is to make sure that the correct packages are installed before organizing the data

ipak(packages)

## now that all the packages are loaded we can begin to sort the data

#Make sure that UCI HAR dataset is n the working directory of the program else you will run into an error

## looking at the X_test file we can see that it is a .txt file with rows of numbers we can use the read.table function

x_te<-read.table("./UCI HAR Dataset/test/X_test.txt")

# we will do the same for the y_test.txt and subject_test.txt as well

y_te<-read.table("./UCI HAR Dataset/test/y_test.txt")
sub_te<-read.table("./UCI HAR Dataset/test/subject_test.txt")

# in the zip file there is a txt file that explains what the y-test and the subject_test file represent
# since the x_test file is the test set and needs the proper labels we will add the correct variable names
# the features.txt files has all the names that we will need to give the x_test data set

feat_names<-read.table("./UCI HAR Dataset/features.txt")[,2] # second coloumn contains all the names
names(x_te)<-feat_names

# if you look at the features.txt files you can see that some of the values contain the string "mean" and "std"
# we will need to isolate the columns that contain these values
#using the grepl function we can see which columns match the string mean or std

ms_feat<-grepl("mean|std",feat_names)

# get the values for all the means and std values in the X_ test dataset

x_te = x_te[,ms_feat]

# the y_test.txt file is a list of all the activities we will use the activity_labels.txt to change the names of the y_test file

act_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")[,2] # this is a factor with 6 levels

y_te[,2]<-act_labels[y_te[,1]] #each level in the y_test dataset gets assigned to one of the activies from the activity_labels.txt

names(y_te)<-c("act_ID","act_label") # add names to the data frame

names(sub_te)<- "subject" # name the subject_test.txt file

# now that we have given names to all the columns, "filtered" all information except mean and std 
# we can now merge all the columns using cbind

tedata<-cbind(as.data.frame(sub_te),y_te,x_te)

# everything we did for the test data will now be done for the training data

X_tr <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_tr <- read.table("./UCI HAR Dataset/train/y_train.txt")
sub_tr <- read.table("./UCI HAR Dataset/train/subject_train.txt")

names(X_tr) <- feat_names
X_tr = X_tr[,ms_feat]

y_tr[,2]<-act_labels[y_tr[,1]]
names(y_tr)<-c("act_ID","act_label")

names(sub_tr)<- "subject"

trdata<-cbind(as.data.frame(sub_tr),y_tr,X_tr)

#Now that both the test and the training data are in a similar formate we can bind then using rbind

totaldata<-rbind(tedata,trdata)

#if we were to print the data now it would still be disorganized, to make it presentable we can use the melt function

# the ideal way to present the data is have subject, activity ID, activity label, variable and the value as colounm names


id_lab= c("subject","act_ID","act_label")
data_lab = setdiff(colnames(totaldata), id_lab)
totalmelt<- melt(totaldata,id=id_lab,measure.vars = data_lab)

# the end goal of this project is to have the mean of the mean and std for each axis for each activity for each subject


clean_data<- dcast(totalmelt,subject + act_label ~ variable, mean)

write.table(clean_data, file = "./tidy_data.txt", row.names = FALSE)




