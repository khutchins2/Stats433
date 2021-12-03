library(tidyverse)
library(stargazer)
library(ggplot2)
library(splitstackshape)
library(dplyr)
library(Matrix)
library(factoextra)
library(RSpectra)
library(spam)
library(MatrixExtra)
library(ramify)
library(data.table)
library(doBy)

# Create data files using: https://www.kaggle.com/jfenley/spotify-sparse

pairs = read.csv("Data/sparse_pairs.csv")

# Convert to sparse matrix. By adding x=1, we make the matrix have
# numeric values instead of boolean values. This is needed for eigs
dt = sparseMatrix(i=pairs$playlist, j=pairs$song_i, index1=F, x=c(1))

rm(pairs) # Want to remove pairs from memory (almost 1GB)
gc() # Even through reference to pairs was removed, need to free memory

sum(dt) # Should equal ~65M

###################### Train/Valid/Test Splits #################################
set.seed(101)

sample <- sample.int(n = nrow(dt), size=900000, replace=F)

train <- dt[sample, ]
test <- dt[-sample, ]

sample2 <- sample.int(n = nrow(test), size=50000, replace=F)
valid <- test[sample2, ]
test <- test[-sample2, ]

nrow(train)
nrow(valid)
nrow(test)

rm(dt, sample, sample2)
gc()

###############################################################################

get.betas <- function(mat, k){
  system.time({
    s = svds(L, k = k)
    X = s$u
    beta = s$v %*% diag(s$d)
    vm = varimax(X)
    nice_X = X %*%vm$rotmat
    nice_beta = beta%*%vm$rotmat
  })
  return (list('X' = nice_X, 'beta' = nice_beta))
}

# Creates two data structures:
# - major: a sparse matrix where each song is removed from each playlist
# - minor: a data table indicating which song was removed from each playlist
shuffle.items <- function(mat, n){
  major = mat 
  
  summ = summary(valid)
  loo = summ %>% group_by(i) %>% sample_n(1) # Leave-one-out from each row
  
  #minor = sparseMatrix(i=loo$i, j=loo$j, x=loo$x, dims=dim(mat)) # Minor matrix is the one song left out from each row.
  cat('Done creating minor matrix\n')
  
  summ = data.table(summ)
  setkeyv(summ, c("i", "j")) # Set key for O(log n) searches
  cat('Done setting key\n')
  
  for (i in seq_along(loo$i)){ # Remove that one song from ecah row in major matrix.
    if (i %% 5000 == 0){
      cat('At iteration', i, 'of 50000\n')
    }
    row <- loo[i, ]
    summ[.(row$i, row$j), 3] = 0
  }
  major = sparseMatrix(i = summ$i, j = summ$j, x=summ$x, dims = dim(mat))
  return(list('major' = major, 'minor' = loo))
}


##########################      SVD     ################################

rs = rowSums(train)
cs = colSums(train)
L = Diagonal(nrow(train), 1/sqrt(rs+mean(rs)))%*%train%*%Diagonal(ncol(train), 1/sqrt(cs + mean(cs)))


res = get.betas(L, 10)
X = res$X
betas = res$beta

rm(cs, rs, res)
gc()

# Create two datasets: one with one song per playlist, one with all but one
system.time({
  mats = shuffle.items(valid, 1)
})

test = mats$major
true = mats$minor

rm(mats)
gc()


# Generate predictions (computer seems to be able to handle up to 
# 50-100 playlists at a time, more than that crashes)
preds = test[1:50,] %*% betas %*% t(betas)


# Check predictions.
# TODO: If a prediction can be a song that is already in playlist,
# need to check and remove it.
correct = 0
incorrect = 0
for (chunk in 1:(nrow(test)/50)){
  start = proc.time()
  start_ind = (chunk - 1) * 50 + 1
  end_ind = start_ind + 49
  preds = test[start_ind:end_ind,] %*% betas %*% t(betas)
  for (i in 1:nrow(preds)){
    row = preds[i,]
    true_song = true[start_ind + i - 1, 2]
    guessed = sum(which.maxn(row, 50)  == true_song)
    if (guessed){
      correct = correct + 1
    } else {
      incorrect = incorrect + 1
    }
  }
  cat("Accuracy:", correct / (correct + incorrect), 'N:', correct+incorrect , "Time:", (proc.time()-start)[3],'s.\n')
}

