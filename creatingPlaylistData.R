

playlistInfo = list()
numStart = 1
numEnd = 3

for(x in numStart:numEnd){
  tempnum = as.character(1000 * (x - 1))
  tempnumTwo = as.character(1000 * (x) - 1)
  line = paste("C:\\Users\\kejoh\\Desktop\\Sample\\mpd.slice.", tempnum, "-", tempnumTwo, ".json", sep = "", collapse=NULL)
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
