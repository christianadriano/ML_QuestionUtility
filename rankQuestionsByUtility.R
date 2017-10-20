#Sort questions by utility

install.packages(class);
library(class);
install.packages(gmodels);
library(gmodels)
install.packages(e1071, dependencies=TRUE)
library(e1071)
install.packages(klaR)
library(klaR)

#Obtain the data

# Import data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_VotingAggregation//aggregateAnswerOptionsPerQuestion.R");
summaryTable <- runMain();
#summaryTable <- data.frame(summaryTable);

#I need to guarantee that some examples (i.e., failing methods)
#do not dominate the training or testing sets. To do that, I need to get a 
#close to equal proportion of examples in both sets

#Scramble the dataset before extracting the training set.
set.seed(8850);
g<- runif((nrow(summaryTable))); #generates a random distribution
summaryTable <- summaryTable[order(g),];


#Confidence + Difficulty for 10 answers 
#loop from n=2 to 19 answers (start with a seed size of two answers)
##Monte carlo simulation (1000)
###sample n answers from ALL questions
###compute utility_confidence_div_difficulty
###computeranking based on utility
###get top three questions 
####Montecarlo Simulation (1000 times)
####loop sample 2 to 19 answers from the top three questions
####append these answers to each of the three questions
#####compute ranking 
#####get top two questions and compute TP, FP, FN, TN
#####compute precision, recall, sentitivity, sensibility
####average the outcomes over the 1000 datapoints
####store it.
##average across 

#Simplified form
#Confidence + Difficulty for 10 answers 
###computeranking based on utility
###get top three questions 
###get another 5 answers from these questions
####append these answers to each of the three questions
#####compute aggregation metric ranking  and compute TP, FP, FN, TN
####store it.
###computer utility again and repeat until there aren't more answers available. (ignore questions that were all asked)
##average across 





#Rank question based on confidence only, difficulty only, confidence/difficulty
#Look that the past, because I don't have the answer yet when I am deciding which question to ask

Monte Carlo
# Sample answer from top question
# 

#After I rank, what do I do?
#Compute the metric after every new answer (MonteCarlo simulation)


#duration + explanation size


#code size + complexity


#profession 

