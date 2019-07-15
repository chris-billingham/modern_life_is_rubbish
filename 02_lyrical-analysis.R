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

# so definitely Leisure had shorter (lyrically) tracks, with great escape being a peak, 
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

# super interesting, i forgot blur like do the odd instrumental/near-instrumental, also
# me, white noise on think tank is mostly phil daniels talk/rapping over the top of 
# the track. also technically it's a hidden track as well :winking emoji:
# let me remove those to get a better view of actual lyrics. 
# i'm pretty sure the debt collector is just them counting it in, let me check
albums %>% filter(track_title == "The Debt Collector")

# yeah so let's exclude anything with less lyrics that the debt collector
# just to get  a bit of purer view
tokens %>%
  mutate(album = as_factor(album),
         track_title = as_factor(track_title)) %>%
  group_by(album, track_title) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  # this filters from debt collector up to tender
  filter(n > 11, n < 500) %>%
  group_by(album) %>%
  summarise(mean_tokens = mean(n)) %>%
  ungroup()

# yeah that brings up Modern Life and Parklife. So it seems that after the relatively simple
# leisure Damon ramped it right up for the so-called "Life" trilogy. then they almost come down again
# when blur released the magic whip, it was commented at the time that it was a return to some of the
# style of the "life" trilogy which makes sense from a lyrical richness perspective

# another thing is 13 is seen as damon's break album from justine frischman
# maybe sentiment analysis can show this, let's use textdata to access the afinn
# sentiment dictionary which i've had a load of success with in the past

library(textdata)

tokens %>%
  mutate(album = as_factor(album)) %>%
  left_join(lexicon_afinn()) %>%
  mutate(value = case_when(is.na(value) ~ 0,
                           TRUE ~ value)) %>%
  group_by(album) %>%
  summarise(total_sent = sum(value))

# oh cripes, that's quite the difference, so again there is an implicit positivity throughout
# the life trilogy whereas everything afterwards is relatively neutral. let's see what this looks like
# on a per-track basis

tokens %>% 
  mutate(album = as_factor(album),
         track_title = reorder_within(track_title, -track_n, album)) %>%
  left_join(lexicon_afinn()) %>%
  mutate(value = case_when(is.na(value) ~ 0,
                           TRUE ~ value)) %>%
  group_by(album, track_title) %>%
  summarise(sent = sum(value)) %>%
  ggplot(aes(track_title, sent, fill = album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album, scales = "free_y") +
  coord_flip() + 
  scale_x_reordered() +
  labs(y = "Nubmer of tokens per track",
       x = NULL,
       title = "What are the most 'wordy' Blur album tracks")


curl::curl_download("http://saifmohammad.com/Lexicons/NRC-Emotion-Lexicon-v0.92.zip",
                    "data/nrc_lex.zip",
                    quiet = FALSE)

dir_ls("data")
dir_ls("data", recursive = TRUE)
nrc_lexicon <- read_table2("data/NRC-Emotion-Lexicon-v0.92/NRC-emotion-lexicon-wordlevel-alphabetized-v0.92.txt",
                           col_names = FALSE,
                           skip = 45)

colnames(nrc_lexicon) <- c("word", "emotion", "value")

nrc_filtered <- nrc_lexicon %>% 
  filter(!(value == 0))

tokens %>%
  mutate(album = as_factor(album)) %>%
  inner_join(nrc_filtered) %>%
  group_by(album, emotion) %>%
  summarise(total_sent = sum(value)) %>%
  ungroup() %>%
  ggplot(aes(emotion, total_sent, fill = album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album) +
  coord_flip()

  



