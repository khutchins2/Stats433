library("rjson")
library("tidyverse")
library("spotifyr")
start_time = Sys.time()

# Should have one input after from 1 to 100

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  #stop("Supply percentage of files to look at.", call.=FALSE)
} 
#How many playlists to go through
numStart = 1
numEnd = 3 #floor(args[0] * 1000)

#Tibble of songs in form of (track_uri  artist_uri  album_uri duration)
songData = tibble(
  track_uri = character(),
  artist_uri = character(),
  album_uri = character(),
  duration = numeric()
)

#List of playlist tibbles
playlistInfo = list()

#for each file
for(x in numStart:numEnd){
  tempnum = as.character(1000 * (x - 1))
  tempnumTwo = as.character(1000 * (x) - 1)
  line = paste("/Users/kevinhutchins/Desktop/Stat433/ProjectFinal/spotify_million_playlist_dataset/data/mpd.slice.", tempnum, "-", tempnumTwo, ".json", sep = "", collapse=NULL)
  input = fromJSON(file = line)

  
  #for each playlist
  for(y in 1:length(input$playlists[])){
    
    #creating temp playlist tibble
    playlist = tibble(
      track_uri = character()
    )
    
    #for each song
    for(z in 1:length(input$playlists[[y]]$tracks[])){
      
      #add song to song list
      temp = data.frame(track_uri = input$playlists[[y]]$tracks[[z]]$track_uri,
                        artist_uri=input$playlists[[y]]$tracks[[z]]$artist_uri, 
                        album_uri=input$playlists[[y]]$tracks[[z]]$album_uri,
                        duration=input$playlists[[y]]$tracks[[z]]$duration_ms /60000)
      
      #add song to songData
      songData = songData %>% add_row(temp)
      
      #add song to playlist tibble
      playlist = playlist %>% add_row(track_uri = input$playlists[[y]]$tracks[[z]]$track_uri)
      
      print("parsing song...")
    }
    
    #add playlist tibble to playlistData
    playlistInfo = c(playlistInfo, playlist)
    
    print("finished parsing playlist")
  }
  
  print("finished parsing file")
}
playlistData <- tibble(
             data = playlistInfo)

for(i in 1:length(data$data[])){
  playlistData$data[i] = as_tibble(playlistData$data[i]$track_uri)
}

songData = songData %>% mutate(track_uri = substr(track_uri, 15, 40), artist_uri = substr(artist_uri, 16, 40), album_uri = substr(album_uri, 15, 40))




clientId = "abb0f2503c27448b9c53f509d4112949"
clientSecret = "742582b3ead642e0b2ccd825fda0fe44"

access_token = get_spotify_access_token(client_id = clientId, client_secret = clientSecret)


songData = songData %>%  add_column(danceability = 0, energy = 0, loudness = 0, speechiness = 0, acousticness = 0, instrumentalness = 0, liveness = 0, valence = 0, tempo = 0)
for(i in 1:length(songData$track_uri)){
  tryCatch({temp = get_track_audio_features(songData$track_uri[i], authorization = access_token)}, error = function(cond) {
    access_token = get_spotify_access_token(client_id = clientId, client_secret = clientSecret)
    },
    finally = {
    access_token = get_spotify_access_token(client_id = clientId, client_secret = clientSecret)
    temp = get_track_audio_features(songData$track_uri[i], authorization = access_token)
  })
  
  songData$danceability[i] = temp$danceability[1]
  songData$energy[i] = temp$energy[1]
  songData$loudness[i] = temp$loudness[1]
  songData$speechiness[i] = temp$speechiness[1]
  songData$acousticness[i] = temp$acousticness[1]
  songData$instrumentalness[i] = temp$instrumentalness[1]
  songData$liveness[i] = temp$liveness[1]
  songData$valence[i] = temp$valence[1]
  songData$tempo[i] = temp$tempo[1]
  Sys.sleep(0.02)
  print("retreiving song")
  
}

end_time = Sys.time()

print(end_time - start_time)

write.csv(songData, "/Users/kevinhutchins/Desktop/Stat433/ProjectFinal/songData" + numStart + "-" + numEnd + ".csv", row.names = FALSE)
#write.csv(playlistData, "/Users/kevinhutchins/Desktop/Stat433/ProjectFinal/playlistData.csv", row.names = FALSE)
# data$data[2]$track_uri[2] gives playlist #2, track #2


