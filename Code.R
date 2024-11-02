# Import packages 
library(tidyverse)
library(tidyr)

# Importing the functions from Lecturer into R
source("./stylometryfunctions.R")

# Importing Data 
DataGPT <- loadCorpus("~/Stylometry 2/functionwords 2/functionwords/GPTfunctionwords/", featureset = "functionwords")
DataHuman <- loadCorpus("~/Stylometry 2/functionwords 2/functionwords/humanfunctionwords/", featureset = "functionwords")


# Creating one matrix for Human work and one matrix for GPT work 
Human_matrix <- NULL
GPT_matrix <- NULL 

for(i in 1:length(DataGPT$features)){
  Human_matrix <- rbind(Human_matrix, DataHuman$features[[i]])
  GPT_matrix <- rbind(GPT_matrix, DataGPT$features[[i]])
}

#creating list with full matrix for human and ChatGPT work 
all_texts <- list(Human_matrix, GPT_matrix)

# Conducting Discriminant Analysis Leave one out cross validation 
LOOCV(discriminantCorpus)

# Creating Confusion Matrix 
confusion_matrix(discriminantCorpus)



