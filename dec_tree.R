dataset <- iris
num_feature = length(dataset[,-(ncol(dataset))])
header <- colnames(dataset)

#----Finding unique values for a column
unique_vals <- function(rows,col){
     return (unique(rows[,col]))
}

#-----Counts the number of each type of example in a dataset and making a matrix of the class label and initialize it to 0
class_uniq <- unique_vals(dataset,(num_feature)+1)
for(i in 1:length(class_uniq)) {
  
  if(i==1){
     label_vec<- c(0)
  }
  else
  {
    label_vec <- c(label_vec,0)
  }
}
label_matrix <- matrix(c(label_vec),ncol=length(class_uniq))
colnames(label_matrix) <- class_uniq
class_counts<- function(rows){
  rows <- as.data.frame(rows)
  num_feature = length(rows[,-ncol(rows)])
  for (i in 1:nrow(rows)){ 
       lbl = rows[i,num_feature+1]
       if (is.na(lbl))
       {
         next
       }
       else
      { label_matrix[1,lbl]<- label_matrix[1,lbl]+1
      }
  }
  return(label_matrix)
}

check_numeric <- function(input) {
  
  return (is.numeric(input) || is.integer(input) || is.double(input))
}

source("D:/data mining/tutPoint/Question.R")

partition <- function(example,ques) {
  true_list <-  matrix(ncol=num_feature+1,byrow=TRUE)
  false_list <- matrix(ncol=num_feature+1,byrow=TRUE)
  example <- as.data.frame(example)
  for (i in 1: nrow(example)) {
     if ( (match(ques,example[i,])) == TRUE ) {# THE TRUE BLOCK
       if(i==1){
           true_list <- matrix( c(unname(unlist(example[i,]))),ncol=num_feature+1,byrow = TRUE)
          
      } 
      else{
        updated_list <- matrix(c(unname(unlist(example[i,]))),ncol=num_feature+1,byrow = TRUE)
        if(nrow(true_list)==1 && is.na(true_list[1,1] ))
        {
          true_list <- updated_list
        }
        else
        {
          true_list <- rbind(true_list,updated_list)
        }
      }
    }
    
    else { # THE FALSE BLOCK
      if(i==1){
        false_list <- matrix(c(unname(unlist(example[i,]))),ncol=num_feature+1,byrow = TRUE)
      } 
      else{
        updated_false_list <- matrix(c(unname(unlist(example[i,]))),ncol=num_feature+1,byrow = TRUE)
        if(nrow(false_list)==1 && is.na(false_list[1,1] ))
        {
          false_list <- updated_false_list
        }
        else
        {
          false_list <- rbind(false_list,updated_false_list)
        }
      }
    }
  }
  list_data <- list(true_list,false_list)
  return(list_data)
}

#Calculate the Gini Impurity for a list of rows.
gini <- function(rows) {
  
  counts=class_counts(rows)
  impurity=1
  for (lbl in colnames(counts)){
    prob_of_lbl = unname(counts[1,lbl],force=FALSE) / as.double(nrow(rows))
    impurity = impurity - (prob_of_lbl**2)
  }
  #print(impurity)
  return (impurity)
}

#Information Gain.
info_gain <- function(left,right,current_uncertainty) {
  
  p = as.double(nrow(left)) / (nrow(left) + nrow(right))
  return (current_uncertainty - p * gini(left) - (1 - p) * gini(right))
}

find_best_split <- function(rows){
  
  best_gain = 0  # keep track of the best information gain
  best_question = "NA"  # keep train of the feature / value that produced it
  current_uncertainty = gini(rows)
  rows <- as.data.frame(rows)
  num_feature = length(rows[,-ncol(rows)])
  for (col in 1:num_feature){
        #print(col)
        best_gain = 0
        best_question = "NA" 
         for (val in  unique(rows[,col])) {
          
          question = Question(dataset,col, val,"<")
          is_numeric = !(is.factor(rows[,col])|is.logical(rows[,col])|is.character(rows[,col]))
          #print(question)
          partition_result <-  partition(rows, Question(dataset,col, val,"<"))
          true_data <- partition_result[[1]]
          false_data <- partition_result[[2]]
          # Skip this split if it doesn't divide the
          # dataset.
          
          if((nrow(true_data)==0) || (nrow(false_data)==0) )
          {
            next
          }
          else
          {
            # Calculate the information gain from this split
            gain = info_gain(true_data, false_data, current_uncertainty)
             #print(question[1])
             #print(true_data)
             #print(false_data)
             #print(gain)
            if (gain >= best_gain)
            {
              best_gain=gain
              best_question=question
              
             }
          }
         }
        if (col==1){
          var <- data.frame(best_gain,best_col_name=names(rows)[col],question=best_question[1],split_value=best_question[2],is_numeric,col_idx=best_question[3])
        }
        else
        {
          updated_var <- data.frame(best_gain,best_col_name=names(rows)[col],question=best_question[1],split_value=best_question[2],is_numeric,col_idx=best_question[3])
          var <- rbind(var,updated_var)
        }
        
  }
  
  return(var)
}

#selecting the best split Question
best_feature_split <- function(X){
  best_name <- X[(which.max(X[,'best_gain'])),'best_col_name']
  best_result <- X[which.max(X[,'best_gain']),]
  best_result<- as.data.frame(best_result)
  return(best_result)
}


build_tree <- function(TreeNode){
  
  #print(nrow(TreeNode$dataset))
  all_feature_best_split_results <- find_best_split(TreeNode$dataset)
  TreeNode$bestSplit<- best_feature_split(all_feature_best_split_results)
  
  TreeNode$info_gain <- TreeNode$bestSplit$best_gain

  if(TreeNode$info_gain > 0 && nrow(TreeNode$dataset) > 20){ 
    partition_result <- partition(TreeNode$dataset, Question(TreeNode$dataset,strtoi(TreeNode$bestSplit[,'col_idx'])[1],as.numeric(as.character(TreeNode$bestSplit$split_value)),'<'))
    print("Spilt success")
    true_data <- partition_result[[1]]
    false_data <-  partition_result[[2]]
    colnames(true_data) <- header
    colnames(false_data) <- header
    print(TreeNode$bestSplit)
    print(nrow(TreeNode$dataset))
    print("after split")
    print(nrow(true_data))
    print(nrow(false_data))
    
    leftTreeNode <- list(dataset="NULL",bestSplit="NULL",info_gain="NULL",leftChild="NULL",rightChild="NuLL")
    leftTreeNode$dataset <- true_data
    #TreeNode$leftchild <- leftTreeNode  
    
    rightTreeNode <-list(dataset="NULL",bestSplit="NULL",info_gain="NULL",leftChild="NULL",rightChild="NuLL")
    rightTreeNode$dataset <- false_data
    #TreeNode$rightchild <- rightTreeNode
 
    TreeNode$leftChild <- build_tree(leftTreeNode) 
    TreeNode$rightChild <- build_tree(rightTreeNode)
    
    
    return (TreeNode)
  }
  else 
  {
    return(TreeNode)
  }
}
rootTreeNode <- list(dataset="NULL",bestSplit="NULL",info_gain="NULL",leftChild="NULL",rightChild="NuLL")
rootTreeNode$dataset <- dataset #set : dataset parameter of this node
resultant <- build_tree(rootTreeNode)


#predicting class for a test row
prediction <- function(node,test_row) {

  
  if( ((node$leftChild=="NULL") || (node$rightChild=="NULL") ))
  { 
    #cat("\n\nTHIS IS A leaf NODE------------------>\n")
    #print(nrow(node$dataset))
    #print(node$bestSplit)
    #cat("Classes : \n")
    #print (class_counts(node$dataset))
    return(class_counts(node$dataset))  
    
  }
  else
  {
    #print(node$bestSplit)
    #print(nrow(node$dataset))
    feature_col <- strtoi(node$bestSplit[,'col_idx'])[1]
    feature_split_value <- as.numeric(as.character(node$bestSplit$split_value))
    test_row_feature_value <- test_row[1,feature_col]
    
    #print(feature_split_value)
    #print(test_row_feature_value)
    #print("------")
    if(test_row_feature_value < feature_split_value) #go to left child
    {
      left <- node$leftChild
      return (prediction(left,test_row))
    }
    else(test_row_feature_value >= feature_split_value) #go to right child
    {
      
      right <- node$rightChild
      return(prediction(right,test_row))
    }
  }
  
    
}

#prediction
test_row <- dataset[130,]
node <- resultant
predicted_classes <- prediction(node,test_row)
prediction_class_name <- names( which.max(as.data.frame(predicted_class)) )

#looping over the dataset and predicting the class using our decision tree 

