
clientId = "abb0f2503c27448b9c53f509d4112949"
clientSecret = "742582b3ead642e0b2ccd825fda0fe44"

Sys.setenv(SPOTIFY_CLIENT_ID = clientId)
Sys.setenv(SPOTIFY_CLIENT_SECRET = clientSecret)

access_token = get_spotify_access_token(client_id = clientId, client_secret = clientSecret)

songData = songData %>%  add_column(danceability = 0, energy = 0, loudness = 0, speechiness = 0, acousticness = 0, instrumentalness = 0, liveness = 0, valence = 0, tempo = 0)

for(x in 1:length(songData$track_uri)){
  
  temp = get_track_audio_features(songData$track_uri[i], authorization = access_token)
  
  songData$danceability[i] = temp$danceability[i]
  songData$energy[i] = temp$energy[i]
  songData$loudness[i] = temp$loudness[i]
  songData$speechiness[i] = temp$speechiness[i]
  songData$acousticness[i] = temp$acousticness[i]
  songData$instrumentalness[i] = temp$instrumentalness[i]
  songData$liveness[i] = temp$liveness[i]
  songData$valence[i] = temp$valence[i]
  songData$tempo[i] = temp$tempo[i]
  Sys.sleep(0.02)
  print("retreiving song")
}

end_time = Sys.time()

print(end_time - start_time)
# data$data[2]$track_uri[2] gives playlist #2, track #2




