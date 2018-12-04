#install.packages("devtools")
library(devtools)
#install.packages("Rcpp")
library(Rcpp)
#install_github("tiagomendesdantas/Rspotify")
library(Rspotify)
#install.packages("plyr")
library(plyr)
#install.packages("dplyr")
library(dplyr)
#install.packages("httr")
library(httr)
#install.packages("readr")
library(readr)
#install.packages("rvest")
library(rvest)
#install.packages("tidyr")
library(tidyr)
#devtools::install_github("josiahparry/geniusR")
library(geniusR)
#install.packages("tidyverse")
library(tidyverse)
library(stringr)
#install.packages("stringdist")
library(stringdist)
#install.packages("tidytext")
library(tidytext)
#install.packages("text2vec")
library(text2vec)
#install.packages("topicmodels")
library(topicmodels)
#install.packages("widyr")
library(widyr)
#install.packages("utf8")
library(utf8)
#install.packages("textcat")
library(textcat)

# Importing the artiste
wikipedia_artiste = read.csv('Nigerian Artiste.txt')

keys <- spotifyOAuth("Rspotify","769ef3519e8444238fde9c8981c6371c","b17e4a7ca0b4426f9962645ba5c74a63")

#Get artiste id
list_of_artiste_id = list()

for (i in 1:nrow(wikipedia_artiste)){
  Catchit <- searchArtist(artistName = wikipedia_artiste[i,],token=keys)
  list_of_artiste_id <- append(list_of_artiste_id, Catchit)
}

id_index <- seq(2, length(list_of_artiste_id), 6)

ID <- list_of_artiste_id [id_index]

artistename_index <- seq(1, length(list_of_artiste_id), 6)

Artiste_Name <- list_of_artiste_id [artistename_index]

#Creating the artiste dataframe
artiste_df <- data.frame(Artiste_id = matrix(unlist(ID)), Name = matrix(unlist(Artiste_Name)))

#Get top songs
extendedArtisteSongs = data.frame()
for (i in 1:nrow(artiste_df)){
  top_songs <- getTop(id = artiste_df[i,1], country = "US", token=keys)
    if (nrow(top_songs) > 0){
      df <- data.frame(song_id=top_songs$id,song_name=top_songs$name,artist_id=artiste_df[i,1])
      extendedArtisteSongs <- rbind(extendedArtisteSongs,df)
    }
}

#Get release year from web api
get_song_year <- function(song_id){
  req <- httr::GET(paste0("https://api.spotify.com/v1/tracks/", song_id), httr::config(token = keys))
  json <- httr::content(req)
  song_date <- json$album$release_date
  return(song_date)
}

#Adding the release date to the song data frame
extendedArtisteSongs$release_dates = ""

for (i in 1:nrow(extendedArtisteSongs)) {
  release_year <- get_song_year(extendedArtisteSongs[i,1])
  extendedArtisteSongs$release_dates[i] <- release_year
}

#Getting the audio features of the songs
features_df  <- data.frame()

for (i in 1:nrow(extendedArtisteSongs)){
  features <- getFeatures(extendedArtisteSongs[i,1], token=keys)
  features_df <- rbind(features_df, features)
}

#Piping the song and features data frames
songfeatures_df <- extendedArtisteSongs %>% 
  left_join(features_df, by = c("song_id" = "id")) %>% 
    left_join(artiste_df, by = c("artist_id" = "Artiste_id"))

#Getting the song lyrics
get_song_lyrics <- function(Artist,Song){
  
  query=paste0("q=",Song, " ", Artist)
  query = URLencode(query)
  token="FM1jJPq-7qGQKsPNbxghaGALhi2oVjQKKvJZ0QMazpnzIQEJzGKRdtccxoyA0Z49"
  req=httr::GET(paste0('https://api.genius.com/search?',query),httr::add_headers(Authorization=paste("bearer",token)) )
  json1 <- httr::content(req)
  
      if (length(json1$response$hits) == 0){
      query=paste0("q=",songfeatures_df[i,2])
      query = URLencode(query)
      token="FM1jJPq-7qGQKsPNbxghaGALhi2oVjQKKvJZ0QMazpnzIQEJzGKRdtccxoyA0Z49"
      req=httr::GET(paste0('https://api.genius.com/search?',query),httr::add_headers(Authorization=paste("bearer",token)) )
      json1 <- httr::content(req)
      }
  
  url <- try(json1$response$hits[[1]]$result$url, silent = TRUE)
    if (class(url)=="try-error"){
    return("")
    }
  url <- url %>% read_html() %>% html_node(".lyrics p")%>% html_text()
  url <- str_replace_all(url, "\n"," ")
  return(url)

}

#Adding the lyrics to the song data frame
songfeatures_df$lyrics = ""

for (i in 1:nrow(songfeatures_df)) {
  song_lyrics <- get_song_lyrics(songfeatures_df[i,20], songfeatures_df[i,2])
  songfeatures_df$lyrics[i] <- song_lyrics
}

#Get artist genre
get_artist_genre <- function(Artiste_id){
  req <- httr::GET(paste0("https://api.spotify.com/v1/artists/", Artiste_id), httr::config(token = keys))
  json <- httr::content(req)
  artist_genres <- json$genres
  artist_genres <- artist_genres %>% unlist(artist_genres) %>% paste(collapse =",")
  return(artist_genres)
}

#Adding the genre to the artiste data frame
songfeatures_df$genres = ""

for (i in 1:nrow(songfeatures_df)) {
  genres <- get_artist_genre(songfeatures_df[i,3])
  songfeatures_df$genres[i] <- genres
}

#Writing the dataframe to csv
write.csv(songfeatures_df, file = "SongFeatures.csv")

# Importing the dataframe
song_data = readr::read_csv('SongFeatures.csv')

song_data_edit <- song_data %>% 
  mutate(Name = str_to_lower(Name)) %>% 
  mutate(Name = str_replace_all(Name,"feat.*", "")) %>%
  mutate(Name = str_replace_all(Name,"ft.*", "")) %>%
  mutate(Name = str_replace_all(Name,"&.*", "")) %>%
  mutate(Name = str_replace_all(Name,"vs.*", "")) %>%
  mutate(Name = str_replace_all(Name,"tekno.*", "tekno")) %>%
  mutate(Name = str_replace_all(Name,"diamond.*", "Diamond Platnumz")) %>%
  mutate(Name = str_trim(Name))

song_data_edit <- song_data_edit %>%
  add_count(Name) %>%
  filter(n>2) %>% 
  rename(vol = n)

#Adding the average release year
song_data_edit$year = as.integer(str_sub(song_data_edit$release_dates,1,4))
song_data_edit$decade = song_data_edit$year - song_data_edit$year %% 10

song_data_avgyear <- song_data_edit %>%
  group_by(Name) %>%
  summarise(avgYear = mean(year)) %>% 
  filter(avgYear >= 2010)

#NLP analysis
lyric_analysis <- song_data_edit%>% 
  unnest_tokens(word,lyrics) %>% 
  anti_join(stop_words) %>%
  count(Name, word, sort = TRUE) %>%
  bind_tf_idf(word, Name, n)
  
lyric_analysis <- lyric_analysis %>% 
  filter(word != 'ooh') %>% 
  filter(word != 'aah') %>% 
  filter(word != 'yeah') %>% 
  filter(!str_detect(word, "[0-9]"))

decade_analysis <- song_data_edit%>% 
  unnest_tokens(word,lyrics) %>% 
  anti_join(stop_words) %>% 
  count(decade, Name, word, sort = TRUE) %>%
  bind_tf_idf(word, decade, n)

decade_analysis <- decade_analysis %>% 
  filter(word != 'ooh') %>% 
  filter(word != 'aah') %>% 
  filter(word != 'yeah') %>% 
  filter(!str_detect(word, "[0-9]"))

decade_words_analysis <- decade_analysis %>% 
  group_by(decade) %>% 
  top_n(5, tf_idf)

write.csv(decade_words_analysis, file = "decade_words_analysis.csv")

decade_similarity_tfidf <- decade_analysis %>% 
  pairwise_similarity(decade, word, tf_idf, upper = FALSE, sort = TRUE)

write.csv(decade_similarity_tfidf, file = "decade_similarity_tfidf.csv")

artiste_similarity <- lyric_analysis %>% 
  pairwise_similarity(Name, word, tf, upper = FALSE, sort = TRUE) %>% 
  filter(item1 %in% song_data_avgyear$Name & item2 %in% song_data_avgyear$Name)

write.csv(artiste_similarity, file = "artiste_similarity.csv")

artiste_similarity_tfidf <- lyric_analysis %>% 
  pairwise_similarity(Name, word, tf_idf, upper = FALSE, sort = TRUE) %>% 
  filter(item1 %in% song_data_avgyear$Name & item2 %in% song_data_avgyear$Name)

write.csv(artiste_similarity_tfidf, file = "artiste_similarity_tfidf.csv")

words_analysis <- lyric_analysis %>% 
  group_by(Name) %>% 
  top_n(3, tf_idf)

write.csv(words_analysis, file = "wordsanalysis.csv")

words_analysis_bigrams <- song_data_edit %>%
  unnest_tokens(bigram, lyrics, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  count(Name, bigram, sort = TRUE) %>% 
bind_tf_idf(bigram, Name, n)

words_analysis_bigrams_output <- words_analysis_bigrams %>% 
  group_by(Name) %>% 
  top_n(3, tf_idf)

#TF analysis
words_analysis_tf <- lyric_analysis %>% 
  group_by(Name) %>% 
  top_n(3, tf)

words_analysis_bigrams_output_tf <- words_analysis_bigrams %>% 
  group_by(Name) %>% 
  top_n(3, tf)

#Writing the dataset to csv
write.csv(words_analysis_bigrams_output_tf, file = "words_analysis_bigrams_output_tf.csv")
write.csv(words_analysis_tf, file = "words_analysis_tf.csv")
write.csv(words_analysis_bigrams_output, file = "words_analysis_bigrams_output.csv")