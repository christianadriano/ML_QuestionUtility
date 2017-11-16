#Sampling approaches

library(dplyr);

#sample N answers with replacement for each question
sampleWithReplacement<- function(questionList, population, sampleSize){
  sample_final<-data.frame();
  for(id in questionList$Question.ID){
    questionSet<-population[population$Question.ID==id,]
    sampled_df<- dplyr::sample_n(questionSet, sampleSize)
    sample_final<-rbind(sample_final,sampled_df);
  }
  return(sample_final);
}


#sample N answers WITHOUT replacement for each question
sampleWithoutReplacement<- function(questionList, answers_df, sampleSize){
}
