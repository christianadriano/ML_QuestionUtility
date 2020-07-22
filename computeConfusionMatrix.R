#Compute True Positives, True Negatives, False Positives, False Negatives
#precision, recall, sensitivity, sensibility

# Initialize Java method questions and bug covering data
initialize<- function(){
  actualBugs <- c(1,4,10,14,20,23,30,32,55,56,57,58,59,72,73,77,84,92,95,97,102,104,115,119,123);
}

computeStatistics<- function(predictedBugs,actualBugs){
  
  statistics_df <- data.frame(matrix(nrow=1,ncol=5))
  colnames(statistics_df)  <- c("precision","recall","sensitivity", "accuracy","answers");
  
  countMatch<- length(match(actualBugs,predictedBugs));
  TP <- countMatch;
  
  FP <- abs(countMatch - length(predictedBugs));
  
  FN <- abs(countMatch - length(actualBugs));
  
  TN <- 129 - TP - FP - FN;
  
  statistics_df$precision <-  (TP/(TP+FP));
  statistics_df$recall <-  (TP/(TP+FN));
  statistics_df$sensitivity <- ((TN)/(FP+TN));
  statistics_df$accuracy <- ((TN+TP)/(FP+TN+TP+FN));
  
  return(statistics_df);
}



