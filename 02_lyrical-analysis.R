library(tidyverse)
library(tidytext)

# read in the lyrics
albums <- readRDS("data/blur_lyrics.rds")

# let's tokenise the lyrics
tokens <- albums %>%
  unnest_tokens("word", "lyric")

# ok something i've wondered around is whether, as blur progressed through their career
# their lyrics became more well more
tokens %>%
  mutate(album = as_factor(album),
         track_title = as_factor(track_title)) %>%
  group_by(album, track_title) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(album) %>%
  summarise(mean_tokens = mean(n)) %>%
  ungroup()

#so definitely Leisure had shorter (lyrically) tracks, with great escape being a peak, 
# weird I never would've thought of that.
# let's look in more detail at it as a per track/album basis
tokens %>% 
  mutate(album = as_factor(album),
         track_title = reorder_within(track_title, -track_n, album)) %>%
  group_by(album, track_title) %>%
  summarise(n = n()) %>%
  ggplot(aes(track_title, n, fill = album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album, scales = "free_y") +
  coord_flip() + 
  scale_x_reordered() +
  labs(y = "Nubmer of tokens per track",
       x = NULL,
       title = "What are the most 'wordy' Blur album tracks")


