#Compute True Positives, True Negatives, False Positives, False Negatives
#precision, recall, sensitivity, sensibility

# Initialize Java method questions and bug covering data
bugCoveringList <- c(1,4,10,14,20,23,30,32,55,56,57,58,59,72,73,77,84,92,95,97,102,104,115,119,123);
totalBugCovering <- length(bugCoveringList);

computeTP<- function(bugCoveringList, predictedBugCovering){
  
  count<- match(bugCoveringList,predictedBugCovering);
  return(count);
}

computeFN<- function(bugCoveringList, predictedBugCovering){
  
  count<- match(bugCoveringList,predictedBugCovering);
  return (length(bugCoveringList) - count);
}

computeFP<- function(bugCoveringList, predictedBugCovering){
  
  count<- match(bugCoveringList,predictedBugCovering);
  return (length(predictedBugCovering) - count);
}

computeTN<- function(bugCoveringList, predictedBugCovering){
  
  return (129 - computeTP(bugCoveringList, predictedBugCovering) + 
            computeFN(bugCoveringList, predictedBugCovering) +
            computeFP(bugCoveringList, predictedBugCovering));
}

computePrecision<- function(bugCoveringList, predictedBugCovering){
  
  TP<-computeTP(bugCoveringList, predictedBugCovering);
  FP<-computeFP(bugCoveringList, predictedBugCovering);
  
  return (TP/(TP+FP));
}

computeRecall<- function(bugCoveringList, predictedBugCovering){
  
  TP<-computeTP(bugCoveringList, predictedBugCovering);
  FN<-computeFN(bugCoveringList, predictedBugCovering);
  
  return (TP/(TP+FN));
}

computeAccuracy<- function(bugCoveringList, predictedBugCovering){
  
  TP<-computeTP(bugCoveringList, predictedBugCovering);
  FP<-computeFP(bugCoveringList, predictedBugCovering);
  FN<-computeFN(bugCoveringList, predictedBugCovering);
  TN<-129-TP-FP-FN;
  
  return ((TP+TN)/(TP+FP+TN+FN));
}

computeSensitivity<- function(bugCoveringList, predictedBugCovering){
  
  TP<-computeTP(bugCoveringList, predictedBugCovering);
  FP<-computeFP(bugCoveringList, predictedBugCovering);
  FN<-computeFN(bugCoveringList, predictedBugCovering);
  TN<-129-TP-FP-FN;
  
  return ((TN)/(FP+TN));
}

computeSensibility<- function(bugCoveringList, predictedBugCovering){
  
  TP<-computeTP(bugCoveringList, predictedBugCovering);
  FN<-computeFN(bugCoveringList, predictedBugCovering);
  
  return ((TP)/(FN+TP));
}

computeOutcomes<- function(predictedBugCoveringList,bugCoveringList){
  
  outcomes<- list(precision=1, recall=0, sensibility=0, sensitivity=0, accuracy=0);

  outcomes$precision <- computePrecision(bugCoveringList,predictedBugCoveringList);
  outcomes$recall <- computeRecall(bugCoveringList,predictedBugCoveringList);
  outcomes$sensible <- computeSensible(bugCoveringList,predictedBugCoveringList);
  outcomes$sensitivity <- computeSensitivity(bugCoveringList,predictedBugCoveringList);
  outcomes$accuracy <- computeAccuracy(bugCoveringList,predictedBugCoveringList);
  outcomesf<-data.frame(outcomes);
  return(outcomesf);
}





