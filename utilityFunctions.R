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
  
  summaryTable["utility"]<-summaryTable$conf.1+2*summaryTable$conf.2+0*summaryTable$conf.3+
    4*summaryTable$conf.4+5*summaryTable$conf.5;
  return(summaryTable);
}

difficulty_utility<-function(df, is_expected_utility){
  
  subset_df<-subset(df,select=c(Answer.difficulty))
  
  #mark all rows that match the selection
  diff.1<- rowSums(subset_df=="1"); 
  subset_df["diff.1"] <- diff.1;
  
  diff.2<- rowSums(subset_df=="2"); 
  subset_df["diff.2"] <- diff.2;
  
  diff.3<- rowSums(subset_df=="3"); 
  subset_df["diff.3"] <- diff.3;
  
  diff.4<- rowSums(subset_df=="4"); 
  subset_df["diff.4"] <- diff.4;
  
  diff.5<- rowSums(subset_df=="5"); 
  subset_df["diff.5"] <- diff.5;
  
  subset_df["QuestionID"] <- df$Question.ID;
  
  subset_df <-subset(subset_df,select= c(QuestionID,diff.1,diff.2,diff.3,diff.4,diff.5));
  
  question_by <- group_by(subset_df,QuestionID);
  summaryTable<- summarize(question_by,
                           Total_1 = sum(diff.1),Total_2 = sum(diff.2), 
                           Total_3 = sum(diff.3),Total_4=sum(diff.4),
                           Total_5 = sum(diff.5));
  
  colnames(summaryTable)<-c("Question.ID","diff.1","diff.2","diff.3","diff.4","diff.5");
  
  summaryTable["utility"]<-(-2)*summaryTable$diff.5+(-1)*summaryTable$diff.4+0*summaryTable$diff.3+
    4*summaryTable$diff.2+5*summaryTable$diff.1;
  
  if(is_expected_utility){#multiply the utility row by the current probability of YES
    summaryTable <- expected_YES(df,summaryTable)
  }
  
  return(summaryTable);
}

expected_YES <- function(df,utility_table) {

  df_yes <- df[c("Question.ID","Answer.option")]
  tb <- table(df_yes)
  df_expected_yes <- data.frame(list("Question.ID" = unlist(row.names(tb)),
                                "Expected.YES" = tb[,"YES"]/rowSums(tb)
                                )
                           )
  
  utility_table$utility <- df_expected_yes$Expected.YES*utility_table$utility
  return(utility_table)
}



