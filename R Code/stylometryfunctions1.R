#install.packages('class')
#install.packages('caret')

library(class)
library(caret)

#load in a literary corpus. Filedir should be the directory of the function words, which contains one folder for
#each author. The 'featureset' argument denotes the type of features that should be used
loadCorpus <- function(filedir,featureset="functionwords",maxauthors=Inf) {
  authornames <- list.files(filedir)
  booknames <- list()
  features <- list()
  count <- 0
  
  for (i in 1:length(authornames)) {
    #print(i)
    if (count >= maxauthors) {break}
    files <- list.files(sprintf("%s%s/",filedir,authornames[i]))
    if (length(files)==0) {next}
    
    firstbook <- FALSE
    booknames[[i]] <- character()
    for (j in 1:length(files)) {
      path <- sprintf("%s%s/%s",filedir,authornames[i],files[j])
      
      fields <- strsplit(files[j],split=' --- ')[[1]]  
      
      if (sprintf("%s.txt",featureset) == fields[2]) {
        booknames[[i]] <- c(booknames[[i]], fields[1])
        count <- count+1
        M <- as.matrix(read.csv(path,sep=',',header=FALSE))  
        if (firstbook == FALSE) {
          firstbook <- TRUE
          features[[i]] <- M
        } else {
          features[[i]]  <- rbind(features[[i]],M)
        }
        
      }
    }
  }
  return(list(features=features,booknames=booknames,authornames=authornames))
}

myKNN <- function(traindata, testdata, trainlabels, k=1) {
  if (mode(traindata) == 'numeric' && !is.matrix(traindata)) {
    traindata <- matrix(traindata,nrow=1)
  }
  if (mode(testdata) == 'numeric' && !is.matrix(testdata)) {
    testdata <- matrix(testdata,nrow=1)
  }
  
  mus <- apply(traindata,2,mean) 
  sigmas <- apply(traindata,2,sd)
  
  for (i in 1:ncol(traindata)) {
    traindata[,i] <- (traindata[,i] - mus[i])/sigmas[i]
  }
  
  for (i in 1:ncol(testdata)) {
    testdata[,i] <- (testdata[,i]-mus[i])/sigmas[i]
  }
  
  preds <- knn(traindata, testdata, trainlabels, k)
  return(preds)
}

discriminantCorpus <- function(traindata, testdata) {
  thetas <- NULL
  preds <- NULL
  probs_matrix <- matrix(0, nrow = nrow(testdata), ncol = length(traindata))
  
  #first learn thea model for each aauthor
  for (i in 1:length(traindata)) {
    words <- apply(traindata[[i]],2,sum)
    
    #some words might never occur. This will be a problem since it will mean the theta for this word is 0, which means the likelihood will be 0 if this word occurs in the training set. So, we force each word to occur at leats once
    inds <- which(words==0) 
    if (length(inds) > 0) {words[inds] <- 1}
    thetas <- rbind(thetas, words/sum(words))
  }
  
  #now classify
  for (i in 1:nrow(testdata)) {
    probs <- NULL
    for (j in 1:nrow(thetas)) {
      probs <- c(probs, dmultinom(testdata[i,],prob=thetas[j,],log=TRUE))
    }
    # Store the probabilities for this test instance in the probs_matrix
    probs_matrix[i, ] <- probs
    # Append the predicted class (index of the max probability)
    preds <- c(preds, which.max(probs))
  }
  return(list(preds, probs_matrix))
}


KNNCorpus <- function(traindata, testdata) {
  train <- NULL
  for (i in 1:length(traindata)) {
    train <- rbind(train, apply(traindata[[i]],2,sum))
  }
  
  for (i in 1:nrow(train)) {
    train[i,] <- train[i,]/sum(train[i,])
  }
  for (i in 1:nrow(testdata)) {
    testdata[i,] <- testdata[i,]/sum(testdata[i,])
  }
  trainlabels <- 1:nrow(train)
  myKNN(train, testdata, trainlabels,k=1)
}

randomForestCorpus <- function(traindata, testdata) {
  x <- NULL
  y <- NULL
  for (i in 1:length(traindata)) {
    x <- rbind(x,traindata[[i]])
    y <- c(y,rep(i,nrow(traindata[[i]])))
  }
  
  for (i in 1:nrow(x)) {
    x[i,] <- x[i,]/sum(x[i,])
  }
  
  for (i in 1:nrow(testdata)) {
    testdata[i,] <- testdata[i,]/sum(testdata[i,])
  }
  
  mus <- apply(x,2,mean)
  sigmas <- apply(x,2,sd)
  for (j in 1:ncol(x)) {
    x[,j] <- (x[,j] - mus[j])/sigmas[j]
    testdata[,j] <- (testdata[,j] - mus[j])/sigmas[j]
  }
  
  y <- as.factor(y)
  rf <- randomForest(x,y)
  
  preds <- numeric(nrow(testdata))
  for (i in 1:nrow(testdata)) {
    preds[i] <- predict(rf,testdata[i,])
  }
  return(list(preds))
}
  
#' K fold Cross Validation
#' Function carries out K fold cross validation for a given model and reports accuracy 
KfoldCrossVal<- function(method,k){
  # creating version fot the data without unknown author 
  data_new <- data[-27, ]

  # initializing vector to hold accuracies 
  accuracies <- numeric(k)
  
  #creating folds 
  folds <- createFolds(1:nrow(data_new), k)
  
  # performing cross validation loop
  for (i in 1:k){
    train_folds <- folds[-i]
    test_folds <- folds[[i]]
    combined_folds <- do.call(c, train_folds)
    test_data <- data_new[0, ]
    train_data <- data_new[0, ]
    
    # create testing and training data 
    test_data <- data_new[test_folds, ]  
    train_data <- data_new[combined_folds, ]  
    test_data <- as.matrix(test_data[, !names(test_data) %in% c("author", "bookname")])
    
    # Initialize a list to store matrices for each author
    author_matrices <- list()
    authors <- M$authornames
    
    # Loop through each unique author
    for (auth in authors) {
      # Extract rows corresponding to the current author
      author_data <- train_data[train_data$author == auth, ]
      
      # Convert the extracted rows to a matrix (excluding the 'author' column itself)
      author_matrix <- as.matrix(author_data[, !names(author_data) %in% c("author", "bookname")])
      
      # Add the matrix to the list, using the author's name as the key
      author_matrices[[auth]] <- author_matrix
    }
    
    # Now use method to determine predictions for testing data 
    preds <- method(author_matrices, test_data)[[1]]
    
    # creating list of true labels of texts
    true_vals <- NULL
    true_authors <- data_new$author[test_folds]
    for(a in true_authors){
      true_vals <- append(true_vals, which(authors == a))
    }
    
    # Calculate and store accuracy
    accuracies[i] <- mean(preds == true_vals)
  }
  
  mean_accuracy <- mean(accuracies)
}

#' Confusion Matrix 
#' This function creates the confusion matrix given a function that returns predictions for each text in a list
#' using training and testing data   

confusion_matrix <- function(method){
  # getting the list of predicted values and taking away unknown authors text
  preds <- method[[1]]
  preds <- preds[-27]
  
  # creating list of true authors for texts (removing unknown authors text)
  true_labels <- factor(data$author[-27])
  
  # labeling each of the predictions by its author 
  pred_labels <- NULL
  for (i in 1:38) {
    pred_labels <- append(pred_labels, M[[3]][-9][preds[i]])
  }
  pred_labels <- factor(pred_labels)
  
  # Creating confusion matrix 
  return(confusionMatrix(pred_labels, true_labels))
}

  

  
