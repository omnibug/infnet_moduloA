# -------------------------
#DATASET: UCI Knowledge Modeling Data Set
# -------------------------
load_data <- function() {
  
  data_dir <- 'data/'
  # train data - used to train the model
  train <<- read.csv(paste(data_dir,'train_data.csv',sep=''))
  # This is how you can control the order of what appears in the boxplot
  train$UNS <<- factor(train$UNS, levels=c("Very Low", "Low", "Middle", "High"))
  # test data - used to test model accuracy
  test <<- read.csv(paste(data_dir,'test_data.csv',sep=''))
  # This is how you can control the order of what appears in the boxplot
  test$UNS <<- factor(test$UNS, levels=c("Very Low", "Low", "Middle", "High"))
  # all data - used for analysis
  full  <<- bind_rows(train, test) # bind training & test data
  full$UNS <<- factor(full$UNS, levels=c("Very Low", "Low", "Middle", "High"))
  
  # Feature descriptions
  descr  <<- read.csv(paste(data_dir,'description.csv',sep=''))
  
  #train[,6:6] <<- as.character(factor(train$UNS, levels = c("very_low", "Low", "Middle", "High")))
  #test[,6:6] <<- as.character(factor(test$UNS, levels = c("Very Low", "Low", "Middle", "High")))
  
  testx <<- test[,1:5]
  testy <<- test[,6:6]
  
  print('Data Sets Loaded')
  
}

