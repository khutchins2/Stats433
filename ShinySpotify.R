
library(rjson)
library(spotifyr)
library(httr)
library(stringr)
library(tidyverse)

clientId = "abb0f2503c27448b9c53f509d4112949"
clientSecret = "742582b3ead642e0b2ccd825fda0fe44"
access_token = get_spotify_access_token(client_id = clientId, client_secret = clientSecret)
#get songData
songData = as_tibble(read_csv("/Users/kevinhutchins/Desktop/Stat433/ProjectFinal/songData.csv"))
#get Betas
nice_betas = Matrix(as.matrix(read_csv("/Users/kevinhutchins/Desktop/Stat433/ProjectFinal/nice_Betas.csv")), sparse = TRUE)

#retreives URI of playlist, or returns false
retreivePlaylist = function(creatorName, playlistName) {
  offset = 0
  
  t = try({
    playlistData = real_get_user_playlists(creatorName,
                                         authorization = access_token,
                                         offsetVal = offset,
                                         include_meta_info = TRUE)
    print(playlistData)
    while(length(playlistData$items) != 0){
      offset = offset + 20
      for(i in 1:length(playlistData$items)){
        if(playlistData$items[[i]]$name == playlistName){
          print("dub")
          returnVal[1] = TRUE
          returnVal[2] = playlistData$items[[1]]$uri
          return(returnVal)
        }
      }
      playlistData = real_get_user_playlists(creatorName,
                                           authorization = access_token,
                                           offsetVal = offset,
                                           include_meta_info = TRUE)
    }
  })
    return(c(FALSE, NULL))
}

#takes playlist and converts it into vector
convertPlaylistToVector = function(uri){
  id = substr(uri, 18, 40)
  playlistData = get_playlist_tracks(id, authorization = access_token)
  temp = playlistData$track.uri
  temp = as_tibble(temp) %>% mutate(track_uri = substr(value, 18, 40))%>%select(track_uri)
  
  tempVec = temp %>% left_join(songData) %>% drop_na(songNum) %>% select(songNum)
  
  length(songData$songNum)
  df <- data.frame(matrix(ncol = 0, nrow = length(songData$songNum)))
  df[1] = 0
  
  for(i in 1:length(tempVec$songNum)){
    df[tempVec$songNum[i], 1] = 1
  }
  df = as.matrix(df)
  return(df)
}

#Outputs tibble of predicted songs
predictSong = function(input){
  
  output = t(input) %*% nice_betas %*% t(nice_betas)
  max = -100
  maxVal = 0
  min = 100
  minVal = 0
  for(i in 1:length(output)){
    if(output[i] > max){
      max = output[i]
      maxVal = i
    }
    if(output[i] < min){
      min = output[i]
      minVal = i
    }
  }
  minSong = songData %>% select(songNum = minVal)
  maxSong = songData %>% select(songNum = maxVal)
}



#private function to replace non-working function in spotifyr >:(
real_get_user_playlists <- function(user_id,
                               authorization = "get_spotify_authorization_code()",
                               offsetVal = 0,
                               include_meta_info = FALSE) {
  
  base_url <- 'https://api.spotify.com/v1/users'
  url <- str_glue('{base_url}/{user_id}/playlists')
  
  params = list(access_token = authorization, offset = offsetVal)
  res <- RETRY('GET', url, query = params, encode = 'json')

  stop_for_status(res)
  res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'))
  
  if (!include_meta_info) {
    res <- res$items
  }
  res
}