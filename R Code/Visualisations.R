install.packages("caret")
install.packages("tm")

library(caret)
library(tm)

library(tidyverse)
library(tidyr)

source("./R Code/stylometryfunctions.R")


# Importing the data into R
DataGPT <- loadCorpus("./functionwords 2/functionwords/GPTfunctionwords", featureset="functionwords")
DataHuman <- loadCorpus("./functionwords 2/functionwords/humanfunctionwords", featureset="functionwords")

print(DataHuman)

print(length(DataGPT$features[1]))
# Summing each category's frequent word counts
G <- NULL
H <- NULL
for (i in 1:length(DataGPT$features)) {
  print(i)
  print(DataGPT$features[[i]])
  G <- rbind(G, apply(DataGPT$features[[i]],2,sum))
  H <- rbind(H, apply(DataHuman$features[[i]]))
}
print(G)
