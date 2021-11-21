library("rjson")
library("tidyverse")
library("birankr")
library("Matrix")

start_time = Sys.time()

numStart = 1
numEnd = 1

#Tibble of songs in form of (track_uri  artist_uri  album_uri duration)
songData = tibble(
  track_uri = character(),
  songNum = numeric(),
  track_Name = character(),
  artist_Name = character()
)

#
edgeList = tibble(
  playlistNum = numeric(),
  songID = numeric()
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
                        songNum = 0,
                        track_Name = input$playlists[[y]]$tracks[[z]]$track_name,
                        artist_Name = input$playlists[[y]]$tracks[[z]]$artist_name)
      
      #add song to songData
      songData = songData %>% add_row(temp)
      
      #add song to playlist tibble
      playlist = playlist %>% add_row(track_uri = input$playlists[[y]]$tracks[[z]]$track_uri)
      
      print("parsing song...")
    }
    
    #add playlist tibble to playlistData
    playlistInfo = c(playlistInfo, playlist)
    
    print("finished parsing playlist...")
  }
}
print("finished parsing files...")

songData = songData %>% distinct()
for(i in 1: length(songData$songNum)){
  songData$songNum[i] = i
}
songData = songData %>% mutate(track_uri = substr(track_uri, 15, 40))
#for each playlist
for(i in 1:length(playlistData$data)){
  playlist = as_tibble(playlistData$data[i])
  playlist = playlist %>% left_join(songData)
  
  for(x in 1: length(playlist$track_uri)){
    temp = tibble(
      playlistNum = i,
      songID = playlist$songNum[x]
    )
    edgeList = edgeList %>% add_row(temp) %>% drop_na() %>% distinct()
  }
}
#creating sparse Matrix
smat = sparsematrix_from_edgelist(data = edgeList)
