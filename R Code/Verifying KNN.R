# Importing the functions from Lecturer into R
source("./R Code/stylometryfunctions.R")
source("./R Code/evaluationfunctions.R")

library(caret)
# Importing the data into R
DataGPT <- loadCorpus("./functionwords 2/functionwords/GPTfunctionwords/", featureset="functionwords")
DataHuman <- loadCorpus("./functionwords 2/functionwords/humanfunctionwords/", featureset="functionwords")
#M <- loadCorpus("./FunctionWords 2/", "frequentwords70")


# This part of the code is a very non-rigorous test for the accuracy of KNN
# A Cross validation will follow.

# Splitting the code into testing and training sets
# Logic:
# Sample a book from all of the authors if the data set contains.
# more than one book written by that author.
# Place that sampled book into the testing set.
traindata_GPT <- DataGPT$features
traindata_Human <- DataHuman$features
testdata <- NULL
testlabels <- c()
truth <- c()

print(traindata_GPT)

for (i in 1:length(traindata)){
  # Skipping all categories with only one essay (too small of a sample)
  if (nrow(traindata[[i]]) == 1){
    next
  }
  
  testind <- sample(1:nrow(traindata[[i]]), 1)
  
  # adding this essay to the test set
  testdata <- rbind(testdata, traindata[[i]][testind, ])
  # each of the ___ are included so create vector of ___
  testlabels <- c(testlabels, i)
  
  traindata[[i]] <- traindata[[i]][-testind, ,drop=FALSE]
  
  truth <- c(truth, i)
}

traindata <- DataGPT$features
print(traindata)

predsKNN <- KNNCorpus(traindata, testdata)
truth_factor <- factor(truth, levels = seq(1, 11, 1))

sum(predsKNN==testlabels)/length(testlabels)

# WORKS UP TO HERE

confusionMatrix(predsKNN, truth_factor)

# Doing Cross Validation 
result <- CrossValidate(KNNCorpus, M)

# Getting the accuracy of the method
sum(result$predictions==result$truth)/length(result$truth)

# Re-factoring the predictions and truth vectors so that they go from 1 to 11
predictions_factor <- factor(result$predictions, seq(1:11))
truth_factor <- factor(result$truth, seq(1:11))

# Doing the confusion matrix
confusionMatrix(predictions_factor, truth_factor)
