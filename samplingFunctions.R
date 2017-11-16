#Sampling approaches

library(dplyr);

#sample N answers with replacement for each question
sampleWithReplacement<- function(questionList, answers_df, sampleSize){
  sampled_dataf<-data.frame();
  for(id in questionList$id){
    questionSet<-answers_df[answers_df$Question.ID==id,]
    sampled_df<- dplyr::sample_n(questionSet, sampleSize)
    sampled_dataf<-rbind(sampled_dataf,sampled_df);
  }
  return(sampled_dataf);
}


#sample N answers WITHOUT replacement for each question
sampleWithoutReplacement<- function(questionList, answers_df, sampleSize){
}
