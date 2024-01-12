install.packages('spotidy')
install.packages('DT')


library(spotidy)
library(usethis)    # to set system environment variables
library(spotifyr)   # to access Spotify
library(tidyverse)  # for data wrangling
library(DT)         # for interactive tables
library(stringr)
library(dplyr)
library(plyr)
library(tidyverse)
library(httr)
library(rvest)
library(stringr)
install.packages('ggthemes')
library(ggthemes)
install.packages('tidytext')
library(tidytext)
install.packages('wordcloud')
library(wordcloud)
library(ggridges)
install.packages("wesanderson")
library(wesanderson)
install.packages("yarrr")
library(yarrr)
library(knitr)
install.packages("kableExtra")
library(kableExtra)
install.packages("radarchart")
library(radarchart)
install.packages('devtools')
library(devtools)
devtools::install_github('charlie86/spotifyr')
install.packages('sentify')
library(sentify)

# set up Spotify client ID and client secret
Sys.setenv(SPOTIFY_CLIENT_ID = '5c06cc377da34e4eb44eb6d3779579fe')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'f1f9395e1015400c82ccfdaffbca4784')


#using spoitfyr
dm <- get_artist_audio_features('depeche mode')

dm <- dm %>% filter(album_name == "Violator" | album_name == "Ultra" | 
                      album_name == "Music for the Masses" | album_name == "Sounds of the Universe" |
                      album_name == "Song of Faith and Devotion" | album_name == "Delta Machine") 

str(dm)

my_token <- get_spotify_api_token(client_id = "e151cc594bea4fa3bde142f17b73e8ea", client_secret = "4467158091dc4181ae8c6df779579c34")
usethis::edit_r_environ(scope = "user")

#See only the studio albums on Viewer (extracted from lives compilations, remixes and EPs)
depeche_mode %>%
  filter(!(album_name %in% c("LiVE SPiRiTS SOUNDTRACK", "Live in Berlin Soundtrack"))) %>%
  select(album_name, track_name) %>%
  distinct() %>%
  count(album_name) %>%
  datatable(colnames = c("Album Name", "Number of Tracks"))


depeche_mode <- get_artist_audio_features(
  artist = 'Depeche Mode',
  include_groups = "album"
)

get_artist_audio_features(
  artist = 'Depeche Mode',
  include_groups = 'album',
  authorization = get_spotify_access_token()
)


excluded_words <- c("live", "mix", "remix")
depeche_mode <- depeche_mode %>%
  filter(
    !str_detect(track_name, regex(paste(excluded_words, collapse = "|"), ignore_case = TRUE)) &
      !str_detect(album_name, regex(paste(excluded_words, collapse = "|"), ignore_case = TRUE)) &
      !str_detect(album_name, regex("LiVE SPiRiTS SOUNDTRACK|Live in Berlin Soundtrack", ignore_case = TRUE))
  )

depeche_mode %>%
  select(album_name, track_name) %>%
  distinct() %>%
  count(album_name) %>%
  datatable(colnames = c("Album Name", "Number of Tracks"))




#Tempo Analysis---------------------------------------------------------------------------------------------------------------


#tempo of all the tracks
depeche_mode %>%
  # Check for varying tempos for the same track
  group_by(album_name, track_name) %>%
  ungroup() %>%
  select(album_name, track_name, tempo) %>%
  arrange(album_name, track_name) %>%
  print(n=250)


min_index <- which.min(depeche_mode$tempo)
max_index <- which.max(depeche_mode$tempo)

# Extracting corresponding track names
min_track <- depeche_mode$track_name[min_index]
max_track <- depeche_mode$track_name[max_index]


cat("Minimum Tempo Track:", min_track, "\n")

cat("Maximum Tempo Track:", max_track, "\n")
cat("Minimum Tempo:", depeche_mode$tempo[min_index], "\n")
cat("Maximum Tempo:", depeche_mode$tempo[max_index], "\n")


#shows the overall tempo analysis of all albums by plot
ggplot(depeche_mode, aes(tempo)) +
  geom_histogram(binwidth = 1)


#By looking at the plot-> there some albums having a tempo near zero


depeche_mode %>%
  filter(tempo > 0) %>%
  group_by(album_name, track_name) %>%
  summarise(tempo = round(mean(tempo)),
            .groups = "drop") %>%
  ungroup() %>%
  ggplot(aes(x = tempo, fill = ..x..)) +
  geom_histogram(binwidth = 4, show.legend = FALSE) +
  scale_fill_gradient(low = "#521F64", high = "#E8889C") +
  labs(x = "Beats per minute",
       y = "Number of tracks",
       title = "Tempo of Depeche Mode Tracks") +
  scale_y_continuous(breaks = c(0, 100, 300)) +  # Set custom breaks
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black"),
    text = element_text(color = "white", size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "white", size = 0.2),
    panel.grid.minor.y = element_line(colour = "white", size = 0.2),
    axis.text = element_text(color = "white"),
    plot.title = element_text(hjust = 0.5)
  )


min_tempo <- min(depeche_mode$tempo)
max_tempo <- max(depeche_mode$tempo)

# Print the results
cat("Minimum Tempo:", min_tempo, "\n")
cat("Maximum Tempo:", max_tempo, "\n")


## add it to the last plot created
last_plot() + dm_style


#Track Ratings-------------------------------------------------------------------------------------------------------------------------
#Each track has several ratings: danceability, energy, speechiness, acousticness, instrumentalness, liveness, and valence by the algorithm of spotify

#Ratings of Depeche Mode Albums:

depeche_mode %>%
  select(album_name,track_name, danceability, energy, speechiness:valence) %>%
  datatable()


playlist_attributes <- depeche_mode %>% filter(!(album_name %in% c("LiVE SPiRiTS SOUNDTRACK","Tour Of The Universe: Barcelona 20/21:11:09","101 (Live)","Songs Of Faith And Devotion Live","Touring The Angel: Live In Milan","Live in Berlin Soundtrack"))) %>%
  select(track_name,album_name, danceability, energy, speechiness:valence) %>%
  pivot_longer(cols = danceability:valence,
               names_to = "attribute",
               values_to = "rating")

unique_album_names <- unique(playlist_attributes$album_name)
print(unique_album_names)
cat(unique_album_names)


ggplot(playlist_attributes, aes(x = rating, colour = attribute)) +
  geom_density()

ggplot(playlist_attributes, aes(x = rating, colour = attribute)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~attribute, scales = "free_y", nrow = 2)

##Energy Ratings of the Albums------------------------------------------------------------------
#NOTE: not included
names(depeche_mode)



energy <- depeche_mode %>%
  select(album_name, 
         danceability) %>%
  pivot_longer(cols = liveness:valence,
               names_to = "attribute",
               values_to = "rating")

ggplot(energy, aes(x = rating, y = energy, colour = attribute)) +
  geom_point(alpha = 0.5, show.legend = FALSE) +
  geom_smooth(method = lm, formula = y ~ x, show.legend = FALSE) +
  facet_wrap(~attribute, scales = "free_x", nrow = 2) +
  labs(x = "Attribute Value",
       y = "Track Energy")


#Valence-------------------------------------------------------------------------------
#the album with the lowest valence value=saddesst album

# valence ridge plot (fig.height = 6, fig.width = 6 in an rmd)
depeche_mode %>% ggplot(aes(x = valence, y = album_name, fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 0.9) + 
  scale_fill_gradient(low = "white", high = "purple") + 
  theme_fivethirtyeight() + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  xlim(0,1) +
  ggtitle("Valence values of Depeche Mode's 15 Album's", subtitle = "Based on valence pulled from Spotify's Web API with spotifyr")
  theme(legend.position = "none")

#according to the joyplot,
  
  # Calculate average valence for each album
  average_valence <- depeche_mode %>%
    group_by(album_name) %>%
    summarize(avg_valence = mean(valence))
  
  # Identify the album with the minimum average valence
  saddest_album <- average_valence %>%
    filter(avg_valence == min(avg_valence))
  
  print(saddest_album)
  
  
  
# table: album by mean valence
depeche_mode %>% 
  group_by(album_name) %>% 
  summarise(mean(valence)) %>% 
  arrange(desc(`mean(valence)`)) %>% 
  kable() %>% 
  kable_styling("striped", full_width = F, position = "left") %>% 
  row_spec(row = 1:15, background = "#fffce4", color = "deeppink")



# table: top 5 songs by valence
depeche_mode %>% 
  select(track_name, album_name, valence) %>% 
  top_n(5) %>% 
  arrange(-valence) %>% 
  kable() %>% 
  kable_styling("striped", full_width = F, position = "left") %>% 
  row_spec(row = 1:5, background = "#fffce4", color = "deeppink")



# Find the saddest song (minimum valence)
saddest_song <- depeche_mode %>%
  select(valence, album_name, track_name) %>%
  arrange(valence) %>%
  slice_head(n = 1)

print(saddest_song)


# sonic score graph
pirateplot(valence + danceability + energy ~ album_release_year, depeche_mode,
           pal = c(wes_palettes$GrandBudapest2, wes_palettes$Moonrise3[1:2]), 
           xlab = "album", ylab = "sonic score",
           theme = 0, point.o = 0.7, avg.line.o = 1, jitter.val = .05, 
           bty = "n", cex.axis = 0.6, xaxt = "n") 
axis(1, cex.axis = 0.6, lwd = 0)
legend("topright", c("1: Taylor Swift", "2: Fearless", "3: Speak Now", "4: Red", "5: 1989", "6: reputation"), bty = "n", cex = 0.6) 




#Joyplot-----------------------------------------------------------------------------------


install.packages('ggjoy')
library(ggjoy)

ggplot(depeche_mode, aes(x = valence, y = album_name)) + 
  geom_joy() + 
  theme_joy() +
  ggtitle("Joyplot of Depeche Mode's joy distributions", subtitle = "Based on valence pulled from Spotify's Web API with spotifyr")





#########
#NOTE: not included
# get the ID for Ma Baker by Boney M from the original album

boneyM <- get_artist_audio_features(
  artist = 'Boney M',
  include_groups = "album"
)

btw_id <- boneyM %>%
  filter( 
         album_name == "Ultra") %>%
  pull(track_id)

btw_features <- get_track_audio_features(btw_id)

btw_analysis <- get_track_audio_analysis(btw_id)


#########










