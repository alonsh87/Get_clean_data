run_analysis<-function()
{
  #reading needed files
  training_set<-read.table("UCI HAR Dataset/train/x_train.txt")
  training_labels<-read.table("UCI HAR Dataset/train/y_train.txt")
  test_set<-read.table("UCI HAR Dataset/test/x_test.txt")
  test_labels<-read.table("UCI HAR Dataset/test/y_test.txt")
  activity_labels<-read.table("UCI HAR Dataset/activity_labels.txt")
  names(activity_labels)=c("number","Activity")
  features<-read.table("UCI HAR Dataset/features.txt")
  #names(test_set)<-features[,2]
  subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt")
  subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt")
  #1 Merging test and train data
  test_training_set<-rbind(test_set,training_set)
  names(test_training_set)<-features[,2]
  test_train_set<-rbind(test_set,training_set)
  test_train_labels<-rbind(test_labels,training_labels)
  test_train_subject<-rbind(subject_test,subject_train)
  names(test_train_labels)<-"Activity"
  names(test_train_subject)<-"Subject"
  #2 getting measurements of mean and std
  mean<-grep("mean",features[,2])
  std<-grep("std",features[,2])
  mean_std<-sort(c(mean,std))
  #df<-cbind(test_train_subject,test_train_labels,test_training_set)
  df_mean_std<-test_training_set[,mean_std]
  df_mean_std<-cbind(test_train_subject,test_train_labels,df_mean_std)
  #3 and 4 naming activities in new data set
  new_df<-merge(df_mean_std,activity_labels,by.x = "Activity",by.y="number")
  new_df[,1]<-new_df[,82]
  new_df[82]<-NULL
  #5 getting average of each variable for ech activity
  agg<-aggregate(a[,3:81],by=c(list(a$Subject),list(a$Activity)),mean)
  colnames(agg)[1:2]<-c("Subject","Activity")
  write.table(agg,"step5.txt",row.names = FALSE)
}