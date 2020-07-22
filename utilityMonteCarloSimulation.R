
library(ggplot2)

#Run a Monte Carlo simulation that selects questions based on utility function
#Two nested simulations: sample answers to compute utility and sample answers to compute precision/recall
#Prints charts of number of answers per question and the levels of precision and recall at each iteration

#TODO
#Print results by Java method
#Compute the probability of YES (two alternatives - MarkovChain and Bayesian Update)

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
accumStatistics<- list(precision=0, recall=0, sensitivity=0, accuracy=0, answers=0);
actualBugs <- c(1,4,10,14,20,23,30,32,55,56,57,58,59,72,73,77,84,92,95,97,102,104,115,119,123);
topQuestions <-list();

#number of high utility question values that should be sampled 
#This is same as ranking, it can be that more than one question has the same utility
questionsToSelect <- 3;
#number of answers should sampled from each selected question
answerSamplingStep <- 1;
#How many of the top ranking should be considered to predict bugs
rankingTop <- 2;

#start with one answer
questionID_f <- data.frame(unique(answerPopulation_df$Question.ID));
colnames(questionID_f)<- c("Question.ID");

#Sample with replacement fits the assumptions of infinite population, unknown distribution, 
#and equal probability of any programmer to provide an answer
sampled_dataf<-sampleWithReplacement(questionList=questionID_f,
                                     population = answerPopulation_df, 
                                     sampleSize = answerSamplingStep);

for(i in 1:10){
  #### compute precision recall accuracy sensitivity
  answersByQuestions_df <- runFromSample(sampled_dataf);
  #Select only the question with ranking position equal of higher than rankingTop (see setup above)
  predictedBugs <- answersByQuestions_df[answersByQuestions_df$rankingVote<=rankingTop,];
  statistics_f<- computeStatistics(predictedBugs,actualBugs); #change name of computeOutcomes to computeStat
  statistics_f$answers <- dim(sampled_dataf)[1];
  accumStatistics <- rbind(accumStatistics,statistics_f);
  
  #compute utility
  utility_table<-difficulty_utility(df=sampled_dataf);
  
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

plotOutcomes(utilityType="difficulty",U=questionsToSelect,A=answerSamplingStep,R=rankingTop);

#########################################
#### PLOT RESULTS #######################
plotOutcomes<- function(utilityType,U,R,A){
  
  hyperparam <- paste("U",U,"A",A,"R",R);
  name <- paste(utilityType,i,hyperparam,"line-plot.jpg");
  
  ggplot(accumStatistics,aes(answers)) + 
    geom_line(aes(y=100*precision, colour="precision")) + 
    geom_line(aes(y=100*recall, colour="recall")) + 
    geom_point(aes(x=answers,y=100*precision), shape=1) +
    geom_point(aes(x=answers,y=100*recall), shape=1) + 
    labs(y="%",x="answers") + 
    labs(title =paste("precision, recall by answers",hyperparam));
  
  
  ggsave(filename=name,last_plot(),device = "jpeg", 
         path=paste0(path,"ML_QuestionUtility//utilityPlots//"));
  
  ggplot(sampled_dataf,aes(sampled_dataf$Question.ID)) + 
    geom_histogram(binwidth = 1) +
    labs(title=paste("Answer per Question",hyperparam)) +
    labs(x="Question.ID", y="Total answers") + 
    xlim(c(1,130));
  
  name <- paste(utilityType,i,hyperparam,"hist-plot.jpg");
  ggsave(filename=name,last_plot(),device = "jpeg",
         path=paste0(path,"ML_QuestionUtility//utilityPlots//"));
}
  
#######################################
#Select top questions within each JavaMethod
selectTopQuestionsByJavaMethod <- function (utility_Table,
                                            sampled_dataf,
                                            questionsToSelect){
  topQuestions <- list();
  javaMethodList<- unique(sampled_dataf$FailingMethod);
  
  for(javaMethod in javaMethodList){
    javaMethod_df <- sampled_dataf[sampled_dataf$FailingMethod==javaMethod,];
    questionList <- unique(javaMethod_df$Question.ID);
    
    utilitySelection <- utility_Table[questionList,];
    
    utilityValues <- unique(utilitySelection$utility);
    sizeL<-length(utilityValues);
    if(sizeL>=questionsToSelect){
      #take only the top values
      index <- questionsToSelect;    
    }
    else{
      index <- length(utilityValues); #fewer questions than the questionsToSelect step
    }
    
    utilityValues<-utilityValues[order(utilityValues,decreasing = TRUE)];
    utilityValues <- utilityValues[1:index]; 
    
    #take the top questions by utility and sample another set answers from them
    topQuestions <- rbind(topQuestions, 
                          data.frame(utilitySelection[utilitySelection$utility>=min(utilityValues),]));
  }
  return(topQuestions);
}
