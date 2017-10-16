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

#confidence + difficulty 

#duration + explanation size

#code size + complexity

#profession 