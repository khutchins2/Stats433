library("tidyverse")
library("spotifyr")
library("FNN")
library("rjson")

#Run creatingPlaylistData.R
#Run creatingSampleData.R

#LOAD DATA

  #get PlaylistAverageData
  playlistAverageData = read_csv("C:\\Users\\kejoh\\Desktop\\Sample\\playlistAverageData.csv")

  #get SongData(1-3)
  songData = read_csv("C:\\Users\\kejoh\\Desktop\\Sample\\songData1-3.csv")
  
#TEMP:
  #remove popularity and release_date
  songData = songData %>% select(track_uri, artist_uri, album_uri, duration, danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo)

#GETTING CLUSTER MASTERTRACKS
  
  #Do PCA on playlist Vectors:
  
    #have to remove playlistNum from playlistAverageData
    playlistPrep = playlistAverageData %>% select(average_danceability,
                                                average_energy,
                                                average_loudness,
                                                average_speechiness,
                                                average_acousticness,
                         ,                       average_instrumentalness,
                                                average_liveness,
                                                average_valence,
                                                average_tempo)
    #perform PCA
    playlistPCA = prcomp(playlistPrep, center = TRUE, scale=TRUE)
    
    playlistPCA = playlistPCA$x %>% as_tibble()
  
    #K-means clustering on PCA to get 5 separate clusters of playlists
    kMean = kmeans(playlistPCA, 5, nstart=50,iter.max = 20)
    kMeanClusterVector = as_tibble(kMean$cluster)
    temp = playlistData %>% add_column(kMeanClusterVector)
    
    cluster1Playlists = temp %>% filter(value == 1)
    cluster2Playlists = temp %>% filter(value == 2)
    cluster3Playlists = temp %>% filter(value == 3)
    cluster4Playlists = temp %>% filter(value == 4)
    cluster5Playlists = temp %>% filter(value == 5)
    
    #add songs from playlists into a cluster masterTrack
    
    #cluster 1 mastertrack
    c1MasterTrack = tibble(
      track_uri = character()
    )
    
    for(a in 1:length(cluster1Playlists$data)){
      #for each playlist in playlistData that is in cluster 1, add every song to the master track
      for(x in 1:length(cluster1Playlists$data[a]$track_uri)){
        c1MasterTrack = c1MasterTrack %>% add_row(track_uri = cluster1Playlists$data[a]$track_uri[x])
      }
    }
    c1MasterTrack = c1MasterTrack %>% distinct() %>% transmute(track_uri = substr(track_uri, 15, 40))
    
    #cluster 2 mastertrack
    c2MasterTrack = tibble(
      track_uri = character()
    )
    
    for(a in 1:length(cluster2Playlists$data)){
      #for each playlist in playlistData that is in cluster 2, add every song to the master track
      for(x in 1:length(cluster2Playlists$data[a]$track_uri)){
        c2MasterTrack = c2MasterTrack %>% add_row(track_uri = cluster2Playlists$data[a]$track_uri[x])
      }
    }
    c2MasterTrack = c2MasterTrack %>% distinct() %>% transmute(track_uri = substr(track_uri, 15, 40))
    
    #cluster 3 mastertrack
    c3MasterTrack = tibble(
      track_uri = character()
    )
    
    for(a in 1:length(cluster3Playlists$data)){
      #for each playlist in playlistData that is in cluster 1, add every song to the master track
      for(x in 1:length(cluster3Playlists$data[a]$track_uri)){
        c3MasterTrack = c3MasterTrack %>% add_row(track_uri = cluster3Playlists$data[a]$track_uri[x])
      }
    }
    c3MasterTrack = c3MasterTrack %>% distinct() %>% transmute(track_uri = substr(track_uri, 15, 40))
    
    #cluster 4 mastertrack
    c4MasterTrack = tibble(
      track_uri = character()
    )
    
    for(a in 1:length(cluster4Playlists$data)){
      #for each playlist in playlistData that is in cluster 1, add every song to the master track
      for(x in 1:length(cluster4Playlists$data[a]$track_uri)){
        c4MasterTrack = c4MasterTrack %>% add_row(track_uri = cluster4Playlists$data[a]$track_uri[x])
      }
    }
    c4MasterTrack = c4MasterTrack %>% distinct() %>% transmute(track_uri = substr(track_uri, 15, 40))
    
    #cluster 5 mastertrack
    c5MasterTrack = tibble(
      track_uri = character()
    )
    
    for(a in 1:length(cluster5Playlists$data)){
      #for each playlist in playlistData that is in cluster 1, add every song to the master track
      for(x in 1:length(cluster5Playlists$data[a]$track_uri)){
        c5MasterTrack = c5MasterTrack %>% add_row(track_uri = cluster5Playlists$data[a]$track_uri[x])
      }
    }
    c5MasterTrack = c5MasterTrack %>% distinct() %>% transmute(track_uri = substr(track_uri, 15, 40))
   
#ANALYZING GIVEN PLAYLIST

  #get URI to add playlist to be analyzed
  samplePlaylist = as_tibble(testData$data[2])%>% distinct() %>% transmute(track_uri = substr(track_uri, 15, 40))
  
  #get given playlist average vector
    
    #get song info from spotify
    
    samplePlaylist = samplePlaylist %>%  add_column(danceability = 0,
                                        energy = 0,
                                        loudness = 0,
                                        speechiness = 0,
                                        acousticness = 0,
                                        instrumentalness = 0,
                                        liveness = 0,
                                        valence = 0,
                                        tempo = 0)
    
    clientId = "abb0f2503c27448b9c53f509d4112949"
    clientSecret = "742582b3ead642e0b2ccd825fda0fe44"
    
    access_token = get_spotify_access_token(client_id = clientId, client_secret = clientSecret)
    
    for(i in 1:length(samplePlaylist$track_uri)){
      temp = get_track_audio_features(samplePlaylist$track_uri[i], authorization = access_token)
      
      samplePlaylist$danceability[i] = temp$danceability
      samplePlaylist$energy[i] = temp$energy
      samplePlaylist$loudness[i] = temp$loudness
      samplePlaylist$speechiness[i] = temp$speechiness
      samplePlaylist$acousticness[i] = temp$acousticness
      samplePlaylist$instrumentalness[i] = temp$instrumentalness
      samplePlaylist$liveness[i] = temp$liveness
      samplePlaylist$valence[i] = temp$valence
      samplePlaylist$tempo[i] = temp$tempo
    }
    
    sampleAverages = samplePlaylist %>% summarize(
                              average_danceability = mean(danceability),
                              average_energy = mean(energy),
                              average_loudness = mean(loudness),
                              average_speechiness = mean(speechiness),
                              average_acousticness = mean(acousticness),
                              average_instrumentalness = mean(instrumentalness),
                              average_liveness = mean(liveness),
                              average_valence = mean(valence),
                              average_tempo = mean(tempo))
    
#add to playlist average PCA and see what cluster it is in
    tempPlaylistPrep = playlistPrep %>% add_row(sampleAverages)
    
    tempPlaylistPCA = prcomp(tempPlaylistPrep, center = TRUE, scale=TRUE)
    
    tempPlaylistPCA = tempPlaylistPCA$x %>% as_tibble()
    
    kMean = kmeans(playlistPCA, 5, nstart=50,iter.max = 20)
    
    ClusterNumber = as.integer(as_tibble(kMean$cluster) %>% tail(1))
    
    if(ClusterNumber == 1){
      masterTrack = c1MasterTrack
    }else if(ClusterNumber == 2){
      masterTrack = c2MasterTrack
    }else if(ClusterNumber == 3){
      masterTrack = c3MasterTrack
    }else if(ClusterNumber == 4){
      masterTrack = c4MasterTrack
    }else{
      masterTrack = c5MasterTrack
    }
    
#do 3 nearest neighbors for average with cluster masterTrack
    
    #call PCA on cluster # songs with average
    sampleAverages = sampleAverages %>% rename(danceability = average_danceability,
                                               energy = average_energy,
                                               loudness = average_loudness,
                                               speechiness = average_speechiness,
                                               acousticness = average_acousticness,
                                               instrumentalness = average_instrumentalness,
                                               liveness = average_liveness,
                                               valence = average_valence,
                                               tempo = average_tempo)
    clusterPCAPrep = masterTrack %>% left_join(songData) %>% distinct() %>%
                                                        select(
                                                                   danceability,
                                                                   energy,
                                                                   loudness,
                                                                   speechiness,
                                                                   acousticness,
                                                                   instrumentalness,
                                                                   liveness,
                                                                   valence,
                                                                   tempo
                                                      ) %>% add_row(sampleAverages)
    
    clusterPCA = prcomp(clusterPCAPrep, center = TRUE, scale=TRUE)
    
    clusterPCA = clusterPCA$x %>% as_tibble()
    
    query = unlist(clusterPCA %>% tail(1))
    sample = clusterPCA %>% head(-1) %>% add_column(dist = 0)
    
    #euclidean distance
    for(i in 1:length(sample$PC1)){
      temp = unlist(sample %>% select(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9) %>% slice(i))
      sample$dist[i] = sqrt(sum((query - temp) * (query - temp)))
    }
    
    #find 3 smallest differences between sample and PCAQuery
    smallest = sample %>% arrange(desc(dist))%>%tail(3)
    
    recommendations = tibble(
      track_num = numeric(),
      track_uri = character()
    )
    
    for(i in 1:length(sample$PC1)){
      temp = sample %>% slice(i)
      for(x in 1:3){
        if(identical(smallest%>%slice(x),temp)){
          recommendations = recommendations %>% add_row(track_num = i, track_uri = "")
        }
      }
    }
    recommendations$track_uri = masterTrack %>% slice(recommendations$track_num)%>%pull(track_uri)
    #return them
    clientId = "abb0f2503c27448b9c53f509d4112949"
    clientSecret = "742582b3ead642e0b2ccd825fda0fe44"
    
    access_token = get_spotify_access_token(client_id = clientId, client_secret = clientSecret)
    for(i in 1:3){
      temp = get_track(recommendations$track_uri[i], authorization = access_token)
      print(temp$name)
      print("by")
      print(temp$artists$name)
    }
    print("\n\n\nSAMPLE PLAYLIST \n\n\n")
    for(i in 1:16){
      temp = get_track(samplePlaylist$track_uri[i], authorization = access_token)
      print(temp$name)
      print("by")
      print(temp$artists$name)
    }
   
    
    