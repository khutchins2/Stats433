library(tidyverse)
library(stargazer)
library(ggplot2)
library(splitstackshape)
library(dplyr)
library(Matrix)
library(factoextra)
library(RSpectra)
library(spam)

# Create data files using: https://www.kaggle.com/jfenley/spotify-sparse

pairs = read.csv("Data/sparse_pairs.csv")

# Convert to sparse matrix. By adding x=1, we make the matrix have
# numeric values instead of boolean values. This is needed for eigs
sparse = sparseMatrix(pairs$playlist, pairs$song_i, index1=F, x=c(1))

rm(pairs) # Want to remove pairs from memory (almost 1GB)
gc() # Even through reference to pairs was removed, need to free memory

sum(sparse) # Should equal ~65M

# None of these techniques seem to work as they all try to find a covariance matrix
# which would be 6.6 million songs x 6.6 million songs = 9TB. Need to figure out
# a more efficient way, such as SVD.

# Prcomp method: ISSUE: NOT ENOUGH MEMORY
pca <- prcomp(sparse, retx=FALSE)
# Memory Error

# Eigs method: ISSUE: NOT ENOUGH MEMORY
eigs(sparse, k=5, opts = list(retvec = FALSE))$values

# Eigen Spam method: ISSUE: NON-SQUARE MATRIX
pairs['val'] = 1
colnames(pairs) <- c("i","pass", "j", "val")
pairs$i = pairs$i + 1
pairs$j = pairs$j + 1
vals = eigen.spam(pairs[c('i', 'j', 'val')], only.values=T)
