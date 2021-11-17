library("rjson")
library("tidyverse")
library("spotifyr")
#Preprocessing to get rid of spotify:track part

#create playlistAverageData

playlistAverageData = tibble(
  playlistNum = numeric(),
  average_duration = numeric(),
  average_danceability = numeric(),
  average_energy = numeric(),
  average_loudness = numeric(),
  average_speechiness = numeric(),
  average_acousticness = numeric(),
  average_instrumentalness = numeric(),
  average_liveness = numeric(),
  average_valence = numeric(),
  average_tempo = numeric(),
)

for(i in 1:length(playlistData$data)){
  
  #get rid of spotify:Track for each playlist
  for(x in 1:length(playlistData$data[i]$track_uri)){
    playlistData$data[i]$track_uri[x] = substr(playlistData$data[i]$track_uri[x], 15, 40)
  }
  
  #left join with songData
  temp = as_tibble(playlistData$data[i])
  temp = left_join(temp, songData) %>% distinct()
  temp = temp %>% summarize(aDuration = mean(duration), 
                     aDance = mean(danceability),
                     aEnergy = mean(energy),
                     aLoudness = mean(loudness),
                     aSpeech = mean(speechiness),
                     aAcoustic = mean(acousticness),
                     aInstrument = mean(instrumentalness),
                     aLiveness = mean(liveness),
                     aValence = mean(valence),
                     aTempo = mean(tempo)
                     )
  
  #compute averages, add to playlistAverageData
  playlistAverageData = playlistAverageData %>% add_row(playlistNum = i,
                                                        average_duration = temp$aDuration,
                                                        average_danceability = temp$aDance,
                                                        average_energy = temp$aEnergy,
                                                        average_loudness = temp$aLoudness,
                                                        average_speechiness = temp$aSpeech,
                                                        average_acousticness = temp$aAcoustic,
                                                        average_instrumentalness = temp$aInstrument,
                                                        average_liveness = temp$aLiveness,
                                                        average_valence = temp$aValence,
                                                        average_tempo = temp$aTempo
                                                        )
  
}

#write playlistAverageData to csv
write.csv(playlistAverageData, "/Users/kevinhutchins/Desktop/Stat433/ProjectFinal/playlistAverageData.csv", row.names = FALSE)


