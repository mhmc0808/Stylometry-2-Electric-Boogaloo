CrossValidate <- function(CorpusFunction, data){
  # Initializing the necessary variables
  predictions <- c()
  truth <- c()

  # Looping though all of the authors
  for (i in 1:length(categories)){
    # Looping through all of the books from the authors
    for (j in 1:length(essaynames[[i]])){
      # Checking that the author has written more than 1 book
      if(length(essaynames[[i]]) > 1){
        # Resetting the vector of training set and testing set
        traindata <- features
        testdata <- NULL
        # Selecting the i-th author and j-th book for the testing set
        testdata <- rbind(testdata, traindata[[i]][j, ])
        # Removing the i-th author and j-th book from the training set
        traindata[[i]] <- traindata[[i]][-j, ,drop = FALSE]
        # Predicting the author from testing data using specified model
        pred <- CorpusFunction(traindata, testdata)
        # adding the predictions of the author to a vector
        predictions <- c(predictions, pred)
        # Adding the true author to the vector truth
        truth <- c(truth, i)
      }
    }
  }
  # Returning the predictions from the cross validation
  return(list(predictions = predictions, truth = truth))
}
