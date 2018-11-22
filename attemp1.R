#read the dataset
dataset=read.csv('data.csv',as.is = TRUE)

#splitting the dataset - 60% will be training data and 40 % will be test data
training_rowCount= (nrow(dataset)*0.6)
test_rowCount=nrow(dataset)-training_rowCount
training_dataset= dataset[1:training_rowCount,]
test_dataset=tail(dataset,n=test_rowCount)


source("D:/data mining/tutPoint/naive_bayes.R")
results_training <- My_NaiveBayes(training_dataset)
results_test <- My_NaiveBayes(test_dataset)


function (dataset,lambda=0){
  
  num_feature = length(dataset[,-3])
  num_obs = nrow(dataset)
  num_class=length(unique(dataset[,num_feature+1]))
  #the conditional probilities -->
  for (i in 1:num_feature){
    tab <- table(dataset[,i],dataset[,num_feature+1]) + lambda
    if(i==1) {
      tab_col_sum <- apply(tab,2,sum)
      cond_prob <- sweep(tab,2,tab_col_sum,"/")
    }
    else{
      tab_col_sum <- apply(tab,2,sum)
      cond_prob_updates <- sweep(tab,2,tab_col_sum,"/")
      cond_prob <- rbind(cond_prob,cond_prob_updates)
    }
  }
  
  tab_col_sum = tab_col_sum/sum(tab_col_sum)
  cond_prob_final = rbind(tab_col_sum,cond_prob)
  
  # prediction --->
  
  for (i in 1:num_obs){
    obs <- cond_prob_final [c(as.character(dataset[i,1:num_feature]),"tab_col_sum"),]
    obsprob=apply(obs,2,prod)
    if(i==1)
    {
      pred <- names(which.max(obsprob))
      
    }
    else
    {
      pred <- c(pred,names(which.max(obsprob)))
    }
    
  }
  
  error <- 100 * (1-sum(pred==dataset[,num_feature+1]) / length(dataset[,1]))
  model <- list (cond_prob_final,dataset[,num_feature+1],pred,error)
  return (model)
}