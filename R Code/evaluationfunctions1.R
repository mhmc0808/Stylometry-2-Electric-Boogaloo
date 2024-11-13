CrossValidate <- function(CorpusFunction, data){
  # Initializing the necessary variables
  predictions <- c()
  truth <- c()
  # Removing the unknown observation from
  authornames <- data$authornames[-9]
  features <- data$features[-9]
  booknames <- data$booknames[-9]
  # Looping though all of the authors
  for (i in 1:length(authornames)){
    # Looping through all of the books from the authors
    for (j in 1:length(booknames[[i]])){
      # Checking that the author has written more than 1 book
      if(length(booknames[[i]]) > 1){
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
