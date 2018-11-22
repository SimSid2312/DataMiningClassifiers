My_NaiveBayes<- function (dataset,lambda=0){
  
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
     
       
       if(length(cond_prob_final["tab_col_sum",]) > 1) #More than two class in dataset
       {
         
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
      
      else #only one class in the dataset - unbalanced dataset
        {
          
          if (i==1)
          {
            pred <- unique(dataset[,num_feature+1])
          }
          else
          {
            pred <- c(pred,unique(dataset[,num_feature+1])) 
          }
        }
      }
      
      error <- 100 * (1-sum(pred==dataset[,num_feature+1]) / length(dataset[,1]))
      model <- list (cond_prob_final,dataset[,num_feature+1],pred,error)
      return (model)
}