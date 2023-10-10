## before targets framework

## libraries
library(tidyverse)
library(here)

## read in data
data <- read_csv(here::here("data/traceybit-lastfm.csv"))


## write a code for reading in the data
data_clean <- data %>%
    ## filter out NA data_time
    filter(!is.na(date_time)) %>%
    ## separate date and time in fourth column using stringr::word() function that extracts words in strings
    mutate(date_time = as.POSIXct(date_time, tz = "UTC", format = "%m/%d/%y %H:%M"),
           ## date only
           date = as.Date(str_sub(as.character(date_time), 1, 10)),
           ## add day of week
           day = weekdays(date_time))

## version 2: convert date to PDT
data_clean <- data %>%
    ## filter out NA data_time
    filter(!is.na(date_time)) %>%
    ## separate date and time in fourth column using stringr::word() function that extracts words in strings
    mutate(date_time = as.POSIXct(date_time, tz = "UTC", format = "%m/%d/%y %H:%M"),
           ## convert to PDT
           date_time = with_tz(date_time, tzone = "America/Los_Angeles"),
           ## date only
           date = as.Date(str_sub(as.character(date_time), 1, 10)),
           ## add day of week
           day = weekdays(date_time))

## daily play summary df
plays_artists_df <- data_clean %>%
  ## group by day, number of songs, number of artists
  group_by(date, day) %>%
  summarise(n_plays = n(),
            n_artists = length(unique(artist))) %>%
  ungroup() %>%
  ## add week
  mutate(week = ifelse(date < as.Date("2023-09-30"), 1,
                       ifelse(date >= as.Date("2023-09-30") & date < as.Date("2023-10-07"), 2, 3))) %>%
  pivot_longer(n_plays:n_artists, names_to = "play_type", values_to = "value") %>%
  mutate(play_type = ifelse(play_type == "n_plays", "total song plays", "n unique artists"))

## visualization

listening_heatmap <-
  ggplot(plays_artists_df, aes(y = ordered(day, levels =
                                             c("Saturday", "Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday")),
                               x = week, fill = value)) +
  geom_tile() +
  facet_grid(~play_type) +
  coord_equal(expand = TRUE) +
  labs(x = "week", y = "", fill = "count") +
  scale_fill_gradient(low = "#E8F5E6",
                      high = "#077F09") +
  # guides(show.legend = FALSE) +
  theme_bw() +
  theme(strip.text = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

listening_heatmap


## top artists
top_artist_df <- data_clean %>%
  ## add week
  mutate(week = ifelse(date < as.Date("2023-09-30"), 1,
                       ifelse(date >= as.Date("2023-09-30") & date < as.Date("2023-10-07"), 2, 3))) %>%
  ## number of plays in week
  group_by(week) %>%
  mutate(total_n_plays = n(),
         total_n_artists = length(unique(artist))) %>%
  ungroup() %>%
  mutate(avg_song_p_artist = total_n_plays / total_n_artists) %>%
  ## group by week, artist
  group_by(week, total_n_plays, total_n_artists, avg_song_p_artist, artist) %>%
  summarise(n_artist_plays = n()) %>%
  arrange(week, -n_artist_plays) %>%
  ungroup() %>%
  mutate(rel_plays = n_artist_plays / total_n_plays,
         plays_vs_avg = n_artist_plays / avg_song_p_artist) %>%
  group_by(week) %>%
  mutate(rank = rank(-n_artist_plays)) %>%
  ungroup()


## top artists visualization
top_artist_fig <- top_artist_df %>%
  mutate(week = paste0("week ", week)) %>%
  filter(rank <= 5) %>%
  select(week, artist, n_artist_plays, total_n_plays, plays_vs_avg, rank) %>%
  ggplot(aes(y = fct_reorder(artist, -rank), x = n_artist_plays)) +
  geom_col() +
  facet_wrap(~week, ncol = 1) +
  labs(title = "top artists by week",
       x = "number of plays",
       y = NULL) +
  scale_x_continuous( expand = c(0, 0)) +
  theme_bw() +
  theme(strip.text = element_text(size = 10),
        # axis.text = element_text(size = 16),
        # axis.title = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

top_artist_fig


