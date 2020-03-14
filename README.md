# RMuse: A sentiment analysis of Muse's discography

This project, written primarily in **R**, uses the SpotifyR package to extract data from Spotify's API and create visualizations that give insight into how Muse's discography has evolved over time. The primary packages used include: `ggplot2 dplyr tidyr corrplot`

Future plans involve creating regression models around the highly-correlated numerical features in dataset generated by the correlation plot in the graphics folder. A clustering analysis would also be a good idea after a visual inspection of scatter plots between variables.

## Important Files

Here I include a description of each of the files in the repository as they relate to the project as a whole:

+ api_grabber.R - set the spotify API token and converts the dataframe in R into a csv file 
+ rmuse.R - includes the code for cleaning and filtering features as well as generating visualizations
+ musedata.csv - (99 row x 23 col) the dataset for analysis, for feature explinations please consult spotify's API documentation

+ ValenceGraphic.jpg - a graphic showing the general mood trend of Muse's discography
+ keycount.jpg - a graphic showing the distibution of keys that Muse songs are written in by album

## Data Cleaning 

After pulling the data in from the csv file generated with api_grabber.R the first step is some more specific cleaning of the data. api_grabber has already removed the non-studio albums.

After viewing the data in RStudio 3 problems jump out at me: 1. There are lots of features that we will never have to use in our analysis 2. The version of Simulation Theory that Spotify has is the deluxe version, which contains non-studio songs and 3. There are duplicates due to the way Spotify's API parses through different regions. So it's time to get cleaning.

First to remove the non-studio songs from Simulation Theory.

```R
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
```
Next task is remove the duplicate tracks.

```R
musedata <- musedata[!duplicated(musedata$track_name), ]
```

And finally to remove features we don't need.
```R
musedata <- select(musedata, -artist_id, -album_id, -album_type, -album_images, -album_release_date,
                   -album_release_date_precision, -track_id, -analysis_url, -artists, -available_markets,
                   -disc_number, -explicit, -track_href, -is_local, -track_preview_url, -type, -track_uri
                   )
```

## Sentiment Analysis

In this particular project, instead of doing any feature engineering, we will just be using Spotify's metric of measuring mood: Valence. It is very easy to find the average happiness of each album in our dataset using the *dplyr* package and the manipulation verbs inside.

```R
musedata %>%
  group_by(album_name) %>%
  summarise(avg_joy = mean(valence)) %>%
  arrange(desc(avg_joy))
```
Which gives us the output:
```
# A tibble: 8 x 2
  album_name                       avg_joy
  <fct>                              <dbl>
1 Simulation Theory (Super Deluxe)   0.466
2 Black Holes and Revelations        0.365
3 Drones                             0.325
4 The 2nd Law                        0.286
5 Absolution                         0.249
6 Origin of Symmetry                 0.230
7 Showbiz                            0.230
8 The Resistance                     0.215
```
So with only 8 studio albums it's pretty easy to see which albums are the happiest and which are the saddest. **HOWEVER**, Spotify's valence scale includes double values from `0.0` being the saddest possible value to `1.0` the happiest. From this, it becomes apparent that Muse albums in general are pretty depressing. With "The Resistance" having the lowest mood to "Simulation Theory" having a more neutral mood.

The next 20 lines of code are then spent building a ggplot2 visualization using the pipe operator, and generate the following graphic:

![image](https://github.com/SamSchutz/RMuse/blob/master/Graphics%20and%20Plots/test.jpg "graphic")
