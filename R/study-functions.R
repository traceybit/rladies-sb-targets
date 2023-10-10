## functions for study
## ------------------------------------------------------


## write a code for reading in the data
clean_data <- function(file) {
  
  data <- data %>%
    ## filter out NA data_time
    filter(!is.na(date_time)) %>%
    ## separate date and time in fourth column using stringr::word() function that extracts words in strings
    mutate(date_time = as.POSIXct(date_time, tz = "UTC", format = "%m/%d/%y %H:%M"),
           ## date only
           date = as.Date(date_time),
           ## add day of week
           day = weekdays(date_time))
}
  
# ## version 2: convert date to PDT
# clean_data <- function(file) {
#   
#   data <- data %>%
#     ## filter out NA data_time
#     filter(!is.na(date_time)) %>%
#     ## separate date and time in fourth column using stringr::word() function that extracts words in strings
#     mutate(date_time = as.POSIXct(date_time, tz = "UTC", format = "%m/%d/%y %H:%M"),
#            ## convert to PDT
#            date_time = with_tz(date_time, tzone = "America/Los_Angeles"),
#            ## add day of week
#            day = weekdays(date_time)) 
# }

## number of songs each day of the week
## ------------------------------------------------

## on which day do i listen to more music?
numb_plays_artist_date <- function(data) {
  
  plays_artists_df <- data %>%
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
  
  plays_artists_df 
    
    
}

## most popular artist each day with play count

top_artists <- function(data) {
  
  top_artist_df <- data %>%
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
  
  top_artists

}

## plot number of songs and artist by day
plot_heatmap <- function(plays_artists_df) {
  
listening_heatmap <-  
  ggplot(plays_artists_df, aes(y = ordered(day, levels = 
                                             c("Saturday", "Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday")), 
                               x = week, fill = value)) +
  geom_tile() +
  facet_grid(~play_type) +
  coord_equal(expand = TRUE) + 
    labs(x = "week", y = "", fill = "count") +
    scale_fill_viridis_c() +
    # guides(show.legend = FALSE) +
  theme_bw()

listening_heatmap
  
}

## make table
make_artist_table <- function(top_artist_df) {
  
  top_artist_df %>% 
    filter(rank <= 5) %>%
    select(week, artist, n_artist_plays, total_n_plays, plays_vs_avg, rank) %>%
    kable()
  
}


