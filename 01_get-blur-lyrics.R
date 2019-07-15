library(tidyverse)
# first let's scrape wikipedia for the list of Blur Studio albums

library(rvest)
library(janitor)
url <- "https://en.wikipedia.org/wiki/Blur_discography#Studio_albums"

# a fair amount of jiggery pokery here to get rid of stuff I don't need.
blur_albums <- read_html(url) %>% 
  html_nodes("table.wikitable.plainrowheaders") %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>%
  clean_names() %>%
  slice(2:(n()-1)) %>%
  .[,1]

# let's now get in all the lyrics
library(genius)

# a little function to download the album lyrics but append the album name
lyric_plus_album <- function(album_name) {
  df <- genius_album(artist = "Blur", album = album_name) %>%
    mutate(album = album_name)
  return(df)
}

# get all the albums and their lyrics
albums <- map_dfr(blur_albums, lyric_plus_album)

# save the lyrics off because this takes too long
saveRDS(albums, "data/blur_lyrics.rds")



