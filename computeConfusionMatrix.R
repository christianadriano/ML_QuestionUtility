#Compute True Positives, True Negatives, False Positives, False Negatives
#precision, recall, sensitivity, sensibility

# Initialize Java method questions and bug covering data
initialize<- function(){
  actualBugs <- c(1,4,10,14,20,23,30,32,55,56,57,58,59,72,73,77,84,92,95,97,102,104,115,119,123);
}

computeStatistics<- function(predictedBugs,actualBugs){
  
  statistics_df <- data.frame(matrix(nrow=1,ncol=7))
  colnames(statistics_df)  <- c("precision","recall","sensitivity", "accuracy","answers","mean_precision","mean_recall");
  
  countMatch<- length(match(actualBugs,predictedBugs));
  TP <- countMatch;
  
  FP <- abs(countMatch - length(predictedBugs));
  
  FN <- abs(countMatch - length(actualBugs));
  
  TN <- 129 - TP - FP - FN;
  
  statistics_df$precision <-  (TP/(TP+FP));
  statistics_df$recall <-  (TP/(TP+FN));
  statistics_df$sensitivity <- ((TN)/(FP+TN));
  statistics_df$accuracy <- ((TN+TP)/(FP+TN+TP+FN));
  statistics_df$mean_precision=0;
  statistics_df$mean_recall=0;
  
  return(statistics_df);
}

compute_incremental_mean <- function(n,original_mean,new_datapoint){
  
  return(
    original_mean + (new_datapoint-original_mean)/n
    
  )
}

incremental_variance <- function(n,x,mean, current_variance){
  
  variance_new <- ((n-2)/(n-1)) *current_variance + ((x-mean)^2)/n
  return(variance_new)
}

compute_regret <- function(question_id,actual_bugs){
  
  if(question_id %in% actual_bugs){
    return(1);
  }
  else
    return(0);
}


