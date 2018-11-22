#read the dataset
dataset=read.csv('data.csv',as.is = TRUE)
dataset<- iris
#splitting the dataset - 60% will be training data and 40 % will be test data
training_rowCount= (nrow(dataset)*0.6)
test_rowCount=nrow(dataset)-training_rowCount
training_dataset= dataset[1:training_rowCount,]
test_dataset=tail(dataset,n=test_rowCount)


source("D:/data mining/tutPoint/naive_bayes.R")
results_training <- My_NaiveBayes(training_dataset)
results_test <- My_NaiveBayes(test_dataset)


