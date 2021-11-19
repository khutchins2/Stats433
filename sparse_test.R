library(tidyverse)
library(stargazer)
library(ggplot2)
library(splitstackshape)
library(dplyr)
library(Matrix)

# Create data files using: https://www.kaggle.com/jfenley/spotify-sparse

pairs = read.csv("Data/sparse_pairs.csv")

# Convert to sparse matrix
sparse = sparseMatrix(pairs$playlist, pairs$song_i, index1=F)
rm(pairs) # Want to remove pairs from memory (almost 1GB)
gc() # Even through reference to pairs was removed, need to free memory

sum(sparse) # Should equal ~65M