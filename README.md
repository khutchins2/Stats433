# Stats433
CreatingLargeDataset: Takes data and adds spotify info
CreatingPlaylistData: Creates playlistData which which is a list of playlists with song info
CreatingSampleData: creates sample data I was just using to test stuff
Explanations: right now its just an elbow graph to see how many clusters to make
playlistAverageVectors: creates a list of every playlist with the average of each data point

createModel:

-Basic idea:
  -gets the average vector of each playlist and enacts PCA on them
  -clusters the resulting components into 5 clusters (guessed bc of elbow graph)
  -adds average vector of user-given playlist into vector list of all playlists, PCA and clusters
   again to find which cluster user-given playlist falls into.
  -takes the average vector of the given vector and finds the 3 nearest neighbors when looking at a 
   list of the vector of every song that is in that cluster
