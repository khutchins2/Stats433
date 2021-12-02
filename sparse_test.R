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

sample <- sample.int(n = nrow(dt), size=800000, replace=F)

train <- dt[sample, ]
test <- dt[-sample, ]

sample2 <- sample.int(n = nrow(test), size=100000, replace=F)
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

shuffle.items2 <- function(mat, method, n){
  rowSums = rowSums(mat)
  major = mat
  minor = mat
  for (i in 1:nrow(mat)){
    if (i %% 10 == 0){
      cat('At iteration', i, 'of', nrow(mat), '\n')
    }
    
    pos = which(mat[i,] > 0, arr.ind = T)
    if (method == 'lno'){ # Leave n of the items out
      ind <- sample.int(n = length(pos), size=length(pos)-n, replace=F)
    } else if (method == 'split'){ # Split with prob n into each set      ind <- sample.int(n = length(pos), size=length(pos)*n, replace=F)
    }
    minor[i, ind] = 1
    major[i, setdiff(pos, ind)] = 1
  }
  return(list('major' = major, 'minor' = minor))
}

shuffle.items <- function(mat, method, n){
  rowSums = rowSums(mat)
  major = mat
  
  summ = summary(valid)
  loo = summ %>% group_by(i) %>% sample_n(1) # Leave-one-out from each row
  
  minor = sparseMatrix(i=loo$i, j=loo$j, x=loo$x, dims=dim(mat)) # Minor matrix is the one song left out from each row.
  cat('Done creating minor matrix')
  for (i in seq_along(loo$i)){ # Remove that one song from ecah row in major matrix.
    if (i %% 1000 == 0){
      cat('At iteration', i, 'of 100000\n')
    }
    row <- loo[i, ]
    #summ[row$i, row$j] = 0
    #major[row$i, row$j] = 0
    #set(major, i=row$i, j=row$j, value=0)
    summ[(summ$i == row$i & summ$j == row$j), 3] = 0
    }
  return(list('major' = major, 'minor' = minor))
}



##########################      SVD     ################################

# None of these techniques seem to work as they all try to find a covariance matrix
# which would be 6.6 million songs x 6.6 million songs = 9TB. Need to figure out
# a more efficient way, such as SVD.

rs = rowSums(train)
cs = colSums(train)
L = Diagonal(nrow(train), 1/sqrt(rs+mean(rs)))%*%train%*%Diagonal(ncol(train), 1/sqrt(cs + mean(cs)))


res = get.betas(L, 10)
X= res$X
betas = res$beta

rm(cs, rs, res)
gc()

system.time({
  mats = shuffle.items(valid, 'lno', 1)
})


install.packages("devtools")
devtools::install_github("RoheLab/vsp")
library(vsp)
fa = vsp(dt, rank=30)