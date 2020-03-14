# Creates dataframe with Spotify's song metrics for every Muse song
library(spotifyr)
library(dplyr)

# Setting tokens for Spotify's API
Sys.setenv(SPOTIFY_CLIENT_ID = 'YOUR_TOKEN_HERE')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOUR_TOKEN_HERE')

# Pulling data and filtering the non-studio albums
muse_df <- get_artist_audio_features('Muse')
non_studio_albums <- c('Live at Rome Olympic Stadium', 'HAARP (Live from Wembley Stadium)', 'Hullabaloo Soundtrack (Eastwest Release)')
muse_df <- filter(muse_df, !album_name %in% non_studio_albums)

# Writing to a csv to use elsewhere
muse_df <- apply(muse_df,2,as.character)
write.csv(muse_df, 'musedata.csv')