
library(ggplot2)

"
Run a Monte Carlo simulation that selects questions based on utility function
Two nested simulations: sample answers to compute utility and sample answers to compute precision/recall
Prints charts of number of answers per question and the levels of precision and recall at each iteration

TODO
Print results by Java method
Print regret


"

######################################################################################################

path <- "C://Users//Christian//Documents//GitHub//"

# Initialize methods
source(paste0(path,"ML_QuestionUtility//computeConfusionMatrix.R"));
source(paste0(path,"ML_QuestionUtility//utilityFunctions.R"));
source(paste0(path,"ML_QuestionUtility//samplingFunctions.R"));
source(paste0(path,"ML_VotingAggregation//aggregateVotes.R"));
source(paste0(path,"ML_VotingAggregation//aggregateAnswerOptionsPerQuestion.R"));

# Import data
source(paste0(path,"ML_VotingAggregation//loadAllAnswers.R"));
answerPopulation_df <- loadAnswers("answerList_data.csv");

#Initialize variables
accumStatistics <- data.frame(list(precision=0, recall=0, sensitivity=0, accuracy=0, answers=0,mean_precision=0,mean_recall=0));
actualBugs <- c(1,4,10,14,20,23,30,32,55,56,57,58,59,72,73,77,84,92,95,97,102,104,115,119,123);
topQuestions <-list();

#number of high utility question values that should be sampled 
#This is same as ranking, it can be that more than one question has the same utility
questionsToSelect <- 3;
#number of answers should sampled from each selected question
answerSamplingStep <- 1;
#How many of the top ranking should be considered to predict bugs
rankingTop <- 2;
#Determines if utility will be multiplied by the probability of YES
is_expected_utility=TRUE
#Type of utility function
utility_function="confidence"
#How many times repeats the sampling
Horizon = 20
#Worker Score
score=3

answerPopulation_df <- answerPopulation_df[answerPopulation_df$Worker.score==score,]


#start with one answer
questionID_f <- data.frame(unique(answerPopulation_df$Question.ID));
colnames(questionID_f)<- c("Question.ID");

#Sample with replacement fits the assumptions of infinite population, unknown distribution, 
#and equal probability of any programmer to provide an answer
sampled_dataf<-sampleWithReplacement(questionList=questionID_f,
                                     population = answerPopulation_df, 
                                     sampleSize = answerSamplingStep);

for(i in 1:Horizon){
  #### compute precision recall accuracy sensitivity
  answersByQuestions_df <- runFromSample(sampled_dataf);
  #Select only the question with ranking position equal of higher than rankingTop (see setup above)
  predictedBugs <- answersByQuestions_df[answersByQuestions_df$rankingVote<=rankingTop,];
  statistics_f<- computeStatistics(predictedBugs,actualBugs); #change name of computeOutcomes to computeStat
  statistics_f$answers <- dim(sampled_dataf)[1];
  statistics_f$mean_precision <- compute_incremental_mean(n=dim(accumStatistics)[1],
                                                          original_mean=mean(accumStatistics$precision),
                                                          new_datapoint=statistics_f$precision)
  
  statistics_f$mean_recall <- compute_incremental_mean(n=dim(accumStatistics)[1],
                                                      original_mean=mean(accumStatistics$recall),
                                                      new_datapoint=statistics_f$recall)
  accumStatistics <- rbind(accumStatistics,statistics_f);
  
  #compute utility (or expected utility depending on the flag)
  if(utility_function=="difficulty"){
    utility_table<-difficulty_utility(df=sampled_dataf,is_expected_utility=is_expected_utility);
  }
  else{
    utility_table<-confidence_utility(df=sampled_dataf,is_expected_utility=is_expected_utility);
  }
  #select top questions from each JavaMethod
  topQuestions <- selectTopQuestionsByJavaMethod(utility_table,
                                                 sampled_dataf,
                                                 questionsToSelect)
  
  #sample more answers from these questions
  sampledAnswers_topQuestions<-sampleWithReplacement(questionList=topQuestions,
                                                     population = answerPopulation_df, 
                                                     sampleSize = answerSamplingStep);
  
  ##Append the sampled answers to the exisintg answers of the top questions
  sampled_dataf <- rbind(sampled_dataf,sampledAnswers_topQuestions);
}

plotOutcomes(utilityFunction=utility_function,is_expected_utility=is_expected_utility,
             U=questionsToSelect,A=answerSamplingStep,R=rankingTop,H=Horizon,score=score
             );

#########################################
#### PLOT RESULTS #######################
plotOutcomes<- function(utilityFunction,is_expected_utility,U,R,A,H,score=score){
  
  if(is_expected_utility)
    utilityType <- paste(utilityFunction,"expected utility")
  else
    utilityType <- paste(utilityFunction,"utility")
  
  hyperparam <- paste0(utilityType,",","U",U,",","A",A,",","R",R,",","H",H,",","S",score);
  name <- paste(hyperparam,"line-plot.jpg");
  
  ggplot(accumStatistics,aes(answers)) + 
    geom_line(aes(y=100*mean_precision, colour="precision")) + 
    geom_line(aes(y=100*mean_recall, colour="recall")) + 
    geom_point(aes(x=answers,y=100*mean_precision), shape=1) +
    geom_point(aes(x=answers,y=100*mean_recall), shape=1) + 
    labs(y="%",x="answers") + 
    labs(title =paste0("Mean Precision, Recall: ",hyperparam));
  
  
  ggsave(filename=name,last_plot(),device = "jpeg", 
         path=paste0(path,"ML_QuestionUtility//expected_utility_plots//"));
  
  ggplot(sampled_dataf,aes(sampled_dataf$Question.ID)) + 
    geom_histogram(binwidth = 1) +
    labs(title=paste("Answers/Question: ",hyperparam)) +
    labs(x="Question.ID", y="Total answers") + 
    xlim(c(1,130));
  
  name <- paste(hyperparam,"hist-plot.jpg");
  ggsave(filename=name,last_plot(),device = "jpeg",
         path=paste0(path,"ML_QuestionUtility//expected_utility_plots//"));
}
  
