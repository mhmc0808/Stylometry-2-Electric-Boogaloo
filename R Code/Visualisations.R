install.packages("caret")
install.packages("tm")

library(caret)
library(tm)

library(tidyverse)
library(tidyr)

source("./R Code/stylometryfunctions.R")


# Importing the data into R
DataGPT <- loadCorpus("./functionwords 2/functionwords/GPTfunctionwords/", featureset="functionwords")
DataHuman <- loadCorpus("./functionwords 2/functionwords/humanfunctionwords/", featureset="functionwords")

print(DataGPT$essaynames)

# Summing each category's frequent word counts
G <- NULL
H <- NULL
for (i in 1:length(DataGPT$features)) {
  print(i)
  print(DataGPT$features[[i]])
  G <- rbind(G, DataGPT$features[[i]])
  H <- rbind(H, DataHuman$features[[i]])
}

G_tot <- NULL
H_tot <- NULL

# Creating sums of all GPT and Human function words across all categories#
for (j in 1:ncol(G)) {
  print(j)
  #print(G[,j])
  G_tot[j] <- sum(G[,j])
  H_tot[j] <- sum(H[,j])
}
print(G_tot)

# Standardising this vector
G_tot <- G_tot/sum(G_tot)
H_tot <- H_tot/sum(H_tot)

print(G_tot)
x <- rbind(G_tot, H_tot)
print(x)

# Creates the distance matrix based on these results
d <- dist(x)
print(d)

# Defines points by performing MDS (Multi-Dimensional Scaling)
pts <- cmdscale(d)

# Plots our points, with each point being the name of each author
plot(pts,type='n', main='MDS Plot of Categories in our Corpus', xlab="", ylab="")
text(pts[,1],pts[,2],label=c("GPT Essays", "Human Essays"),cex=0.8)


# MAP OF EACH FUNCTION WORDS FOR GPT AND HUMAN ESSAYS

G_tot <- G_tot[-71]
H_tot <- H_tot[-71]

x_vals <- 1:70

# Plotting the first dataset 'y'
plot(x_vals, G_tot, type="o", col="blue", xlab="Frequent words", ylab="Normalised Values", main="Comparing frequency of function words for GPT and Human-written essays", pch=16)

# Add the second dataset 'unknown' on the same plot
lines(x_vals, H_tot, type="o", col="red", pch=17)

# Adding a legend to differentiate the datasets
legend("topright", legend=c("GPT Essays", "Human Essays"), col=c("blue", "red"), pch=c(16, 17))




# PROBLEM: TOO MANY PLOTS OF DIFFERENT CATEGORIES - CANT REPRESENT ON ONE PAGE,
# WILL HAVE TO USE SHINY ONLINE TO SHOW THIS


# Standardising results so that each category has the same weighting
for (i in 1:nrow(G)) {
  G[i,] <- G[i,] / sum(G[i,])
  H[i,] <- H[i,] / sum(H[i,])
}
for (j in 1:ncol(G)) {
  G[,j] <- (G[,j] - mean(G[,j]))/sd(G[,j])
  H[,j] <- (H[,j] - mean(H[,j]))/sd(H[,j])
}


# Creates the distance matrix based on these results
g <- dist(G)
h <- dist(H)

# Defines points by performing MDS (Multi-Dimensional Scaling)
pts_G <- cmdscale(g)
pts_H <- cmdscale(h)


# Plots our points, with each point being the name of each author
plot(pts_G,type='n', main='MDS Plot of Categories in our Corpus', xlab="", ylab="")
plot(pts_H,type='n', main='MDS Plot of Categories in our Corpus', xlab="", ylab="")
text(pts_G[,1],pts_G[,2],label=DataGPT$categories,cex=0.8)
text(pts_H[,1],pts_H[,2],label=DataHuman$categories, "G",cex=0.8)

