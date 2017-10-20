#Compute different utility values for questions


library(dplyr);

confidence_utility<-function(df){
  
  subset_df<-subset(df,select=c(Answer.confidence))
  
  #mark all rows that match the selection
  conf.1<- rowSums(subset_df=="1"); 
  subset_df["conf.1"] <- conf.1;
  
  conf.2<- rowSums(subset_df=="2"); 
  subset_df["conf.2"] <- conf.2;
  
  conf.3<- rowSums(subset_df=="3"); 
  subset_df["conf.3"] <- conf.3;
  
  conf.4<- rowSums(subset_df=="4"); 
  subset_df["conf.4"] <- conf.4;
  
  conf.5<- rowSums(subset_df=="5"); 
  subset_df["conf.5"] <- conf.5;
  
  subset_df["QuestionID"] <- df$Question.ID;
  
  subset_df <-subset(subset_df,select= c(QuestionID,conf.1,conf.2,conf.3,conf.4,conf.5));
  
  question_by <- group_by(subset_df,QuestionID);
  summaryTable<- summarize(question_by,
                           Total_1 = sum(conf.1),Total_2 = sum(conf.2), 
                           Total_3 = sum(conf.3),Total_4=sum(conf.4),
                           Total_5 = sum(conf.5));
  
  colnames(summaryTable)<-c("Question.ID","conf.1","conf.2","conf.3","conf.4","conf.5");
  
  summaryTable["utility"]<-summaryTable$conf.1+2*summaryTable$conf.2+3*summaryTable$conf.3+
    4*summaryTable$conf.4+5*summaryTable$conf.5;
  return(summaryTable);
}

#sample N answers for each question
sampleAnswers<- function(questionList, answers_df, sampleSize){
  sampled_dataf<-data.frame();
  for(id in questionList$id){
    questionSet<-answers_df[answers_df$Question.ID==id,]
    sampled_df<- sample_n(questionSet, sampleSize)
    sampled_dataf<-rbind(sampled_dataf,sampled_df);
  }
  return(sampled_dataf);
}



source("C://Users//chris//OneDrive//Documentos//GitHub//ML_QuestionUtility//computeConfusionMatrix.R");
#Initialize variables
accumOutcomes<- list();


#start with minimal answers (5 per question)
questionID_f <- data.frame(unique(answers_df$Question.ID));
colnames(questionID_f)<- c("id");
sampled_dataf<-sampleAnswers(questionList=questionID_f,answers_df = dataf, sampleSize = 5);

#### compute precision recall accuracy sensibility sensitivity
bugCoveringPredictedList <- selectPredictedBugs(rankedSelection = sampled_dataf);
outcomesf<- computeOutcomes(bugCoveringPredictedList);
accumOutcomes <- rbind(accumOutcomes,outcomesf);

#compute utility
utility_table<-confidence_utility(df=sampled_dataf);

#sort by utility
utility_table<-utility_table[order(utility_table$utility, decreasing = TRUE),];

#take the top 5 and sample another 5 answers compute
topQuestions<-utility_table[1:5,];

#sample another 5 answers from these questions



