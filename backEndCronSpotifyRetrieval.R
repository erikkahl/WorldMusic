#this code retrieve all the information for spotify and creates and save the matrix that the front end will use for
#data visualization

library(readxl)
library(spotifyr)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(rworldmap)


Sys.setenv(SPOTIFY_CLIENT_ID = 'be188d86ec8b459a9716886d3edee417')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'cf362ac67e064ed48e1427f8ac3a6c42')

access_token <- get_spotify_access_token()

#***Read the URIs of countries 
URICountries <- read_excel("URICountries.xlsx")
View(URICountries)

#***Retrieve list of popular songs in each country 
tracksLists <-lapply(X= URICountries$TopChart_URI, FUN = get_playlist_tracks)

#***Transform this list of data frames into a single data frame 
tracksDF <- bind_rows(tracksLists, .id = "column_label")
Musix= merge(x=URICountries, y=tracksDF, by.x = "Number", by.y = "column_label")


#***Retrieve list of characteristics per song
songs_characteristics<- lapply(X= Musix$track.id, FUN = get_track_audio_features)
class(songs_characteristics) #spotify returns a list 
songs_characteristics_DF <- bind_rows(songs_characteristics, .id = "column_label") #this list is then turn to a Data Frame  which will be used for merging

WorldSongs3= cbind(Musix, songs_characteristics_DF)

# EIRCK's CODE
WorldSongs4 <- WorldSongs3 %>%
  select(Country, danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence,tempo)

IncrementalTable<-c()

for(i in 1:62)
{
  a = 50*(i-1)+1
  b = 50*i
  WorldSongs6 <-WorldSongs4 %>%
    slice(a:b)
  avgener = sum(WorldSongs6$energy) / 50
  avgdance = sum(WorldSongs6$danceability) / 50
  avgacous = sum(WorldSongs6$acousticness) / 50
  avgspeech = sum(WorldSongs6$speechiness) / 50
  avgloud = sum(WorldSongs6$loudness) / 50
  avglive = sum(WorldSongs6$liveness) / 50
  avginstru = sum(WorldSongs6$instrumentalness) / 50
  avgval = sum(WorldSongs6$valence) / 50
  avgtem = sum(WorldSongs6$tempo) / 50
  
  #print(WorldSongs6)
  IncrementalTable <- rbind(IncrementalTable,c(avgener,avgdance,avgacous,avgspeech,avgloud,avglive,avginstru,avgval,avgtem))
}

IncrementalTable <- cbind(IncrementalTable,c(URICountries$Country))

#rownames(IncrementalTable) <- c(URICountries$Country)
colnames(IncrementalTable) <- c("Energy", "Danceability", "Acousticness", "Speechiness", "Loudness", "Liveness", "Instrumentalness", "Valence", "Tempo", "country" )

write.csv(IncrementalTable, "IT.csv")

