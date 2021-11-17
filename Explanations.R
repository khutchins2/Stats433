#Elbow graph to explain how to get number of clusters in PCA
  set.seed(123)
  # Compute and plot wss for k = 2 to k = 15.
  k.max <- 15
  temp = playlistPCA %>% select(PC1, PC2, PC3)
  data <- temp
  wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
  plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

  numClusters = 5
  
  
  #find values of