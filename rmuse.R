# Importing libraries
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(corrplot)
library(viridis)

# Read csv into environment
musedata <- read.csv(file = 'C:/Users/Sambus/Desktop/muse post/musedata.csv')

# Testing a read on the csv
str(musedata)

# Removing tracks attached to the studio albums that aren't actually a "part of the album"
non_studio_songs <- c('Algorithm (Alternate Reality Version)',
                      'The Dark Side (Alternate Reality Version)',
                      'Pressure (feat. UCLA Bruin Marching Band)',
                      'Propaganda (Acoustic)',
                      'Break It to Me (Sam de Jong Remix)',
                      'Something Human (Acoustic)',
                      'Thought Contagion (Live)',
                      'Dig Down (Acoustic Gospel Version)',
                      'The Void (Acoustic)',
                      'The Dark Side (Alternate Reality Version) [Instrumental] - Alternate Reality Version; Instrumental'
                      )
musedata <- filter(musedata, !track_name %in% non_studio_songs)

# Removing duplicate tracks

musedata <- musedata[!duplicated(musedata$track_name), ]

# Removing columns with data not useful for analysis

musedata <- select(musedata, -artist_id, -album_id, -album_type, -album_images, -album_release_date,
                   -album_release_date_precision, -track_id, -analysis_url, -artists, -available_markets,
                   -disc_number, -explicit, -track_href, -is_local, -track_preview_url, -type, -track_uri
                   )

# Begin analysis

# FINDING CORRELATIONS BETWEEN DIFFERENT FEATURES
tiff("corrplot.jpg", units="in", width=13, height=13, res=300)
muse <- cor(musedata[,c(4,5,6,7,9,10,11,12,13,14,15,16)])
corrplot(muse, type = "lower", method = "number",)
#corrplot(X, p.mat = X, insig = "p-value", sig.level = -1, method="shade")
dev.off()


# FINDING WHICH MUSE ALBUM IS THE HAPPIEST AND SADDEST OVERALL
musedata %>%
  group_by(album_name) %>%
  summarise(avg_joy = mean(valence)) %>%
  arrange(desc(avg_joy))

tiff("test.jpg", units="in", width=14, height=8.5, res=300)
musedata %>%
  group_by(album_name, album_release_year) %>%
  summarise(avg_joy = mean(valence)) %>%
  arrange(desc(avg_joy)) %>%
  ggplot(mapping = aes(reorder(album_name, album_release_year), avg_joy)) +
  geom_point(aes(size = 3, colour=album_release_year)) +
  ylim(0.0, 1.03) +
  geom_hline(yintercept=0.50, linetype="dashed", color = "black", size = 1.5) +
  geom_hline(yintercept=1.0, linetype="dashed", color = "black", size = 1.0) +
  geom_hline(yintercept=0.0, linetype="dashed", color = "black", size = 1.0) +
  geom_text(aes("Origin of Symmetry",0.53,label = "Neutral emotion marker")) +
  geom_text(aes("Origin of Symmetry",1.03,label = "Incredible joy")) +
  geom_text(aes("Origin of Symmetry",0.03,label = "Ultimate sadness")) +
  labs(title = "Average sentiment for each Muse album: A progression",
       subtitle = "(Using metrics pulled from Spotify's API)",
       y = "Average valence",
       x = "Album name by release date")
dev.off()

# SEEING WHICH KEY THE MOST MUSE SONGS ARE IN

musedata %>%
  group_by(key_mode) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup()

tiff("keycount.jpg", units="in", width=15, height=8.5, res=300)
musedata %>%
  group_by(key_mode, album_name) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot(aes(key_mode, n)) +
  geom_bar(mapping = aes(x = key_mode, y = n, fill=album_name), stat = "identity") +
  labs(title = "Histogram of key commonality in Muse's discography",
       x = "Song key",
       y = "count")
dev.off()

# REGRESSION ANALYSIS OF VALENCE VS DANCEABLILITY 

ggplot(musedata, aes(energy, loudness)) +
  geom_point(aes(alpha=0.5))