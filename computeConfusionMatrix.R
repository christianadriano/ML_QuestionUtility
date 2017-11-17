#Compute True Positives, True Negatives, False Positives, False Negatives
#precision, recall, sensitivity, sensibility

# Initialize Java method questions and bug covering data
initialize<- function(){
  actualBugs <- c(1,4,10,14,20,23,30,32,55,56,57,58,59,72,73,77,84,92,95,97,102,104,115,119,123);
}

computeStatistics<- function(predictedBugs,actualBugs){
  
  statistics<- list(precision=0, recall=0, sensitivity=0, accuracy=0);
  
  countMatch<- length(match(actualBugs,predictedBugs));
  TP <- countMatch;
  
  FP <- abs(countMatch - dim(predictedBugs)[1]);
  
  FN <- abs(countMatch - length(actualBugs));
  
  TN <- 129 - TP - FP - FN;
  
  statistics$precision <-  (TP/(TP+FP));
  statistics$recall <-  (TP/(TP+FN));
  statistics$sensitivity <- ((TN)/(FP+TN));
  statistics$accuracy <- ((TN+TP)/(FP+TN+TP+FN));
  
  statistics_f <- data.frame(statistics);
  return(statistics_f);
}



