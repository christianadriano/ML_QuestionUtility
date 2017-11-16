


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
accumStatistics<- list();
actualBugs <- c(1,4,10,14,20,23,30,32,55,56,57,58,59,72,73,77,84,92,95,97,102,104,115,119,123);

#start with one answer
questionID_f <- data.frame(unique(answers_df$Question.ID));
colnames(questionID_f)<- c("id");

#Sample with replacement fits the assumptions of infinite population, unknown distribution, 
#and equal probability of any programmer to provide an answer
sampled_dataf<-sampleWithReplacement(questionList=questionID_f,answers_df = answers_df, sampleSize = 1);

#for(i in 1:4){
  #### compute precision recall accuracy sensibility sensitivity
  answersByQuestions_df <- runFromSample(sampled_dataf);
  #Select only the question with ranking position equals 1 or 2
  predictedBugs <- answersByQuestions_df[answersByQuestions_df$rankingVote==1 | answersByQuestions_df$rankingVote==2,];
  statistics_f<- computeStatistics(predictedBugs,actualBugs); #change name of computeOutcomes to computeStat
  accumStatistics <- rbind(accumStatistics,statistics_f);
  
  #compute utility
  utility_table<-confidence_utility(df=sampled_dataf);
  
  #sort by utility
  utility_table<-utility_table[order(utility_table$utility, decreasing = TRUE),];
  
  #take the top 5 and sample another 5 answers compute
  topQuestions<-data.frame(utility_table[1:5,]);
  colnames(topQuestions)<- c("id");
  
  #sample another 5 answers from these questions
  sampledAnswers_topQuestions<-sampleWithReplacement(questionList=topQuestions,answers_df = dataf, sampleSize = 5);
  
  ##Append the sampled answers to the exisintg answers of the top questions
  sampled_dataf <- rbind(sampled_dataf,sampledAnswers_topQuestions);
#}

plot(accumOutcomes);
