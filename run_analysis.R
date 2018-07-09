
# if(!file.exists("./data")){dir.create("./data")}
# fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# download.file(fileUrl,destfile="./data/acc.zip",method="curl")
# unzip("./data/acc.zip",exdir = "./data")
# file.remove("./data/acc.zip")

library(LaF)
#library(tidyr)
library(dplyr)
#library(data.table)

features<-read.csv("./data/UCI HAR Dataset/features.txt",header=F,sep = " ",colClasses = "character")
features$V1<-NULL
names(features)<-"sensordata"

# fix the problem of duplicated names
nv=features$sensordata # names of variables
dup<-duplicated(nv)
und<-unique(nv[dup]) # unique nv dup
for (ii in 1:length(und)) {
  index<-which(nv==und[ii])
  nv[index[1]]<-paste0(nv[index[1]],"-X")
  nv[index[2]]<-paste0(nv[index[2]],"-Y")
  nv[index[3]]<-paste0(nv[index[3]],"-Z")
}
features$sensordata<-nv

#############
### train ###
#############
subject_train<-read.csv("./data/UCI HAR Dataset/train/subject_train.txt",
                header=F,sep = "\n",col.names="subject") # 1~30
#subject_train<-read.table("./data/UCI HAR Dataset/train/subject_train.txt",sep = "\n") # 1~30

fixedwidth=16
N_obs_train=7352
N_col=561 # number of columns
N_col_isig=128 # number of columns for inertial signals

laf <- laf_open_fwf("./data/UCI HAR Dataset/train/X_train.txt", column_widths = rep(fixedwidth,N_col),column_types=rep("double", N_col))
X_train <- laf[,]
#X_train<-read.fwf("./data/UCI HAR Dataset/train/X_train.txt",widths = rep(fixedwidth,N_col),sep = "\n")
#N_obs_train<-dim(subject_train)[1] # number of observations
#X_train<-as.matrix(X_train)
#X_train<-matrix(X_train,nrow=dim(subject_train)[1],ncol=N_col,byrow = T)
dim(X_train)
#X_train<-data.frame(X_train)

names(X_train)<-features$sensordata

activity_labels<-read.csv("./data/UCI HAR Dataset/activity_labels.txt",header=F,sep = " ",colClasses = "character")
activity_labels$V1<-NULL
names(activity_labels)<-"activity_labels"
activity_labels
activity_labels[,]<-tolower(activity_labels[,])

activity_train<-read.csv("./data/UCI HAR Dataset/train/y_train.txt",
                         header=F,sep = "\n",col.names="activity") # activity 1~6

for (ii in 1:dim(activity_labels)[1]){
  actlab<-activity_train[,1]==as.character(ii)
  activity_train[,1][actlab]<-activity_labels[ii,]
  }

train<-cbind(subject_train,activity_train,X_train)

filenames<-list.files("./data/UCI HAR Dataset/train/Inertial Signals")

for (ii in 1:length(filenames)) {
  laf <- laf_open_fwf(paste0("./data/UCI HAR Dataset/train/Inertial Signals/",filenames[ii]), 
                      column_widths=rep(fixedwidth,N_col_isig),column_types=rep("double",N_col_isig))
  data1 <- laf[,]
  
  #data1<-read.fwf(paste0("./data/UCI HAR Dataset/train/Inertial Signals/",filenames[ii]),widths = rep(fixedwidth,N_col_isig),sep = "\n")
  #data1<-as.matrix(data1)
  #data1<-matrix(data1,nrow=N_obs_train,ncol=N_col_isig)
  dim(data1)
  #data1<-data.frame(data1)
  names(data1)<-paste(sub(".txt","",filenames[ii]),sub("V","reading ",names(data1)))
  names(data1)<-sub("_train","",names(data1),fixed = T)
  train<-cbind(train,data1) 
  }
train$train_or_test<-rep("train",N_obs_train)
names(train)

############
### test ###
############
subject_test<-read.csv("./data/UCI HAR Dataset/test/subject_test.txt",
                        header=F,sep = "\n",col.names="subject") # 1~30
#subject_test<-read.table("./data/UCI HAR Dataset/test/subject_test.txt",sep = "\n") # 1~30

laf <- laf_open_fwf("./data/UCI HAR Dataset/test/X_test.txt",
                    column_widths = rep(fixedwidth,N_col),column_types=rep("double", N_col))
X_test <- laf[,]
#X_test<-read.fwf("./data/UCI HAR Dataset/test/X_test.txt",widths = rep(fixedwidth,N_col),sep = "\n")
N_obs_test<-dim(subject_test)[1] # number of observations
#X_test<-as.matrix(X_test)
#X_test<-matrix(X_test,nrow=dim(subject_test)[1],ncol=N_col)
dim(X_test)
#X_test<-data.frame(X_test)
names(X_test)<-features$sensordata

activity_test<-read.csv("./data/UCI HAR Dataset/test/y_test.txt",
                         header=F,sep = "\n",col.names="activity") # activity 1~6

for (ii in 1:dim(activity_labels)[1]){
  actlab<-activity_test[,1]==as.character(ii)
  activity_test[,1][actlab]<-activity_labels[ii,]
}

test<-cbind(subject_test,activity_test,X_test)

filenames<-list.files("./data/UCI HAR Dataset/test/Inertial Signals")

for (ii in 1:length(filenames)) {
  laf <- laf_open_fwf(paste0("./data/UCI HAR Dataset/test/Inertial Signals/",filenames[ii]), 
                      column_widths = rep(fixedwidth,N_col_isig),column_types=rep("double", N_col_isig))
  data1 <- laf[,]
  #data1<-read.fwf(paste0("./data/UCI HAR Dataset/test/Inertial Signals/",filenames[ii]),widths = rep(fixedwidth,N_col_isig),sep = "\n")
  #data1<-as.matrix(data1)
  #data1<-matrix(data1,nrow=N_obs_test,ncol=N_col_isig)
  dim(data1)
  #data1<-data.frame(data1)
  names(data1)<-paste(sub(".txt","",filenames[ii]),sub("V","reading ",names(data1)))
  names(data1)<-sub("_test","",names(data1),fixed = T)
  test<-cbind(test,data1) }

test$train_or_test<-rep("test",N_obs_test)
names(test)

# 1.Merge the training and the test sets to create one data set.
intersect(names(train),names(test))
#options(warning.length = 8000)

#merged<-merge(train,test,by.x="subject",by.y = "activity",all=T)
merged<-merge(train,test,all=T)
#head(merged)
dim(merged)
#quantile(merged$`tBodyAcc-mean()-X_train`,probs = seq(0,1,length= 5),na.rm = T)
#table(merged$subject)

# merged is the wanted dataset. 

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
#index<-grep("\\bmean()\\b",names(merged))
index<-grep("mean()",names(merged),fixed = T)
datawithmean<-merged[,index]
names(datawithmean)

index<-grep("\\bstd()\\b",names(merged))
datawithstd<-merged[,index]
names(datawithstd)

# datawithmean and datawithmean are the wanted measurements.

# 3.Uses descriptive activity names to name the activities in the data set
merged$activity<-sub("_"," ",merged$activity)

# merged$activity is the wanted names.

# 4.Appropriately labels the data set with descriptive variable names.
features_info<-readLines("./data/UCI HAR Dataset/features_info.txt")
features_info[33:49]
index<-grep("():",features_info,fixed = T)
features_info[index]

splited_feature_info<-strsplit(features_info[index],"\\: ")
splited_feature_info<-unlist(splited_feature_info)

for (ii in 1:(length(splited_feature_info)/2)) {
  ind<-grep(splited_feature_info[2*ii-1],names(merged),fixed = T)
  names(merged)[ind]<-sub(splited_feature_info[2*ii-1],splited_feature_info[2*ii],
                          names(merged)[ind],fixed = T)
}
names(merged)<-sub(".","",names(merged),fixed = T)

ind<-grep("^f",names(merged))
names(merged)[ind]<-sub("f","frequency domain ",names(merged)[ind])
names(merged)[ind]

#names(merged)[grepl("^t", names(merged))]
ind<-grepl("^t", names(merged)) & !grepl("^total", names(merged)) & !grepl("^train", names(merged))
names(merged)[ind]<-sub("t","time domain ",names(merged)[ind])
names(merged)[ind]

names(merged)<-gsub("total","total ",names(merged),fixed = T)
names(merged)[grep("total",names(merged))]

names(merged)<-gsub("body","body ",names(merged),fixed = T)
names(merged)[grep("body",names(merged))]
names(merged)<-gsub("Body","body ",names(merged),fixed = T)
names(merged)[grep("body",names(merged))]

names(merged)<-sub("Acc","accelerometer ",names(merged),fixed = T)
names(merged)<-sub("_acc","accelerometer ",names(merged),fixed = T)
names(merged)[grep("accelerometer",names(merged))]

names(merged)<-sub("Gyro","gyroscope ",names(merged),fixed = T)
names(merged)<-sub("_gyro","gyroscope ",names(merged),fixed = T)
names(merged)[grep("gyroscope",names(merged))]

names(merged)<-sub("Jerk","Jerk signal ",names(merged),fixed = T)
names(merged)[grep("Jerk signal",names(merged))]

names(merged)<-sub("Mag","magnitude ",names(merged),fixed = T)
names(merged)[grep("magnitude",names(merged))]

names(merged)<-sub("angle(t","angle(time domain ",names(merged),fixed = T)
names(merged)[grep("angle(time domain",names(merged),fixed = T)]

names(merged)<-sub("gravity","gravity ",names(merged),fixed = T)
names(merged)[grep("gravity",names(merged))]

names(merged)<-gsub("Mean","mean",names(merged),fixed = T)
names(merged)[grep("mean",names(merged))]

names(merged)<-sub("Auto","auto",names(merged),fixed = T)
names(merged)[grep("auto",names(merged))]

ind<-grepl("Burg order equal to 4", names(merged))
names(merged)[ind]<-sub("Burg order equal to 4","Burg order equal to 4, coefficient ",names(merged)[ind])
names(merged)<-sub("coefficient  X axis ,","X axis, coefficient ",names(merged))
names(merged)<-sub("coefficient  Y axis ,","Y axis, coefficient ",names(merged))
names(merged)<-sub("coefficient  Z axis ,","Z axis, coefficient ",names(merged))
names(merged)[ind]

# ind<-grep("-[0-9],[0-9]",names(merged))
# names(merged)[ind]

names(merged)<-sub("-X"," X axis ",names(merged),fixed = T)
names(merged)<-sub("_x"," X axis ",names(merged),fixed = T)
names(merged)[grep("X axis",names(merged))]
names(merged)<-sub("-Y"," Y axis ",names(merged),fixed = T)
names(merged)<-sub("_y"," Y axis ",names(merged),fixed = T)
names(merged)[grep("Y axis",names(merged))]
names(merged)<-sub("-Z"," Z axis ",names(merged),fixed = T)
names(merged)<-sub("_z"," Z axis ",names(merged),fixed = T)
names(merged)[grep("Z axis",names(merged))]

#names(merged)<-sub("-"," ",names(merged),fixed = T)
names(merged)<-gsub("  "," ",names(merged),fixed = T)

# 5.From the data set in step 4, creates a second, independent tidy data set 
#   with the average of each variable for each activity and each subject.
Ncomb<-(max(merged$subject)*dim(activity_labels)[1])
Nc<-dim(merged)[2]-1
avgdata<-merged[1:Ncomb,1:Nc]
avgdata[,]<-NA

Ni<-max(merged$subject)
Nj<-(dim(activity_labels)[1])
for (ii in 1:Ni){
  for (jj in 1:Nj){
    kk<-(ii-1)*Nj+jj
    avgdata$subject[kk]<-ii
    avgdata$activity[kk]<-activity_labels[jj,]
    
    filtered_data<-filter(merged,merged$subject==ii & merged$activity==activity_labels[jj,])
    avgdata[kk,3:Nc]<-as.numeric(colMeans(filtered_data[,3:Nc]))
  }
}

names(avgdata)[3:Nc]<-paste("avg",names(avgdata)[3:Nc])

# avgdata is the wanted tidy dataset.
write.table(avgdata,"./data/avgdata.txt",row.names = F)

#source("./run_analysis.R")
#system.time(source("./run_analysis.R"))
