
library(ggplot2)

#Run a Monte Carlo simulation that selects questions based on utility function
#Two nested simulations: sample answers to compute utility and sample answers to compute precision/recall

######################################################################################################

# Initialize methods
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_QuestionUtility//computeConfusionMatrix.R");
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_QuestionUtility//utilityFunctions.R");
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_QuestionUtility//samplingFunctions.R");
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_VotingAggregation//aggregateVotes.R");
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_VotingAggregation//aggregateAnswerOptionsPerQuestion.R");

# Import data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_VotingAggregation//loadAllAnswers.R");
answers_df <- loadAnswers("answerList_data.csv");

#Initialize variables
accumStatistics<- list(precision=0, recall=0, sensitivity=0, accuracy=0, answers=0);
actualBugs <- c(1,4,10,14,20,23,30,32,55,56,57,58,59,72,73,77,84,92,95,97,102,104,115,119,123);
topQuestions <-list();

#number of questions with the highest utility values should be sampled
questionsToSelect <- 2;
#number of answers should sampled from each selected question
answerSamplingStep <- 2;
#How many of the top ranking should be considered to predict bugs
rankingTop <- 2;

#start with one answer
questionID_f <- data.frame(unique(answers_df$Question.ID));
colnames(questionID_f)<- c("Question.ID");

#Sample with replacement fits the assumptions of infinite population, unknown distribution, 
#and equal probability of any programmer to provide an answer
sampled_dataf<-sampleWithReplacement(questionList=questionID_f,
                                     population = answers_df, 
                                     sampleSize = answerSamplingStep);
#sampled_dataf<-sampled_dataf[sampled_dataf$FailingMethod=="HIT01_8",];

for(i in 1:4){
  #### compute precision recall accuracy sensitivity
  answersByQuestions_df <- runFromSample(sampled_dataf);
  #Select only the question with ranking position equal of higher than rankingTop (see setup above)
  predictedBugs <- answersByQuestions_df[answersByQuestions_df$rankingVote<=rankingTop,];
  statistics_f<- computeStatistics(predictedBugs,actualBugs); #change name of computeOutcomes to computeStat
  statistics_f$answers <- dim(sampled_dataf)[1];
  accumStatistics <- rbind(accumStatistics,statistics_f);
  
  #compute utility
  utility_table<-confidence_utility(df=sampled_dataf);
  
  #select top questions from each JavaMethod
  topQuestions <- selectTopQuestionsByJavaMethod(utility_table,sampled_dataf,questionsToSelect)
  
  #sample more answers from these questions
  sampledAnswers_topQuestions<-sampleWithReplacement(questionList=topQuestions,
                                                     population = answers_df, 
                                                     sampleSize = answerSamplingStep);
  
  ##Append the sampled answers to the exisintg answers of the top questions
  sampled_dataf <- rbind(sampled_dataf,sampledAnswers_topQuestions);
}

##########################
#PLOT RESULTS
ggplot(accumStatistics,aes(x=accumStatistics$answers,y=accumStatistics$precision)) + 
  geom_point(shape=1) +
  geom_smooth();

ggplot(sampled_dataf,aes(sampled_dataf$Question.ID)) + 
  geom_histogram();
##########################

##########################
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
    
    utilityValues <- utilityValues[1:index]; 
    
    #take the top questions by utility and sample another set answers from them
    topQuestions <- rbind(topQuestions, 
                          data.frame(utilitySelection[utilitySelection$utility>=min(utilityValues),]));
  }
  return(topQuestions);
}
