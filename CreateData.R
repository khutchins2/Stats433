library(data.table)
library(tidyverse)
library(dplyr)
library(tidytext)
library(Matrix)
library(rARPACK)
library(glmnet)
library(rjson)
library(birankr)


start_time = Sys.time()

numStart = 1
numEnd = 10

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

#createSongData
songData = songData %>% mutate(track_uri = substr(track_uri, 15, 40))
#write songData
write_csv(songData, "/Users/kevinhutchins/Desktop/Stat433/ProjectFinal/songData.csv")

#for each playlist
for(i in 1:length(playlistInfo)){
  playlist = as_tibble(playlistInfo[i]$track_uri) %>% mutate(track_uri = substr(value, 15, 40))%>%select(track_uri)
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

# dt your data matrix.
rs = rowSums(smat)
cs = colSums(smat)
Final = Diagonal(nrow(smat), 1/sqrt(rs+mean(rs)))%*%smat%*%Diagonal(ncol(smat), 1/sqrt(cs + mean(cs)))

samp = svds(Final, k = 30)
kval = 6

varX = samp$u[,1:kval]
betas = samp$v[,1:kval]%*%diag(samp$d[1:kval])

varm= varimax(varX)
nice_xs = varX%*%varm$rotmat

#createBetas
nice_betas = betas%*% varm$rotmat
#write betas to file
write_csv(as.data.frame(nice_betas), "/Users/kevinhutchins/Desktop/Stat433/ProjectFinal/nice_Betas.csv")

