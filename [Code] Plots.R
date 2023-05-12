Spotify <- read.csv("D:/Information Visualization/Spotify.csv")
View(Spotify)
str(Spotify)

Spotify$mode <- as.factor(Spotify$mode)
Spotify$key <- as.factor(Spotify$key)
Spotify$explicit <- as.factor(Spotify$explicit)
Spotify$release_date <- as.data.frame(Spotify$release_date)

Billboard <- read.csv("D:/Information Visualization/Billboard.csv")
View(Billboard)
str(Billboard)

Billboard$mode <- as.factor(Billboard$mode)
Billboard$key <- as.factor(Billboard$key)
Billboard$explicit <- as.factor(Billboard$explicit)
Billboard$release_date <- as.data.frame(Billboard$release_date)




#Figure 1 Statistical Relationship Between Some Track Attributes
Billboard_ntrack = subset(Billboard, select = -c(track_title, id, b_rank, b_year, 
                                                 release_date, artist_1, 
                                                 artists_2, artists_3, artists_4, 
                                                 artists_5, artists_6, year, duration_ms))
View(Billboard_ntrack)              
library(GGally)
ggpairs(Billboard_ntrack)


#Figure 2 The Correlations Between Some Track Attributes
#data preparation
attach(s)
df <- Spotify[,c(4,5,7,9,11,12,15,16,17,6,14)]
head(df)
#matrix
df_cor <- round(cor(df),2)
head(df_cor)
df_melt<-melt(df_cor)
head(df_melt)
#tri
df_cor[upper.tri(df_cor)]<-NA
head(df_cor)
df_cor %>%
  melt(na.rm = TRUE)%>%
  ggplot(aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+
  scale_fill_gradient2(low='grey',high='blue',mid='white',
                       midpoint = 0,limit = c(-1,1), space = "Lab", name="Correlation")+
  coord_fixed()
#
df_cor <- round(cor(df),2)
dd<-as.dist((1 - df_cor)/2)
hc <-hclust(dd)
df_cor < df_cor[hc$order,hc$order]
df_cor[upper.tri(df_cor)]<-NA
heatmap_1 <- df_cor %>%
  melt(na.rm = TRUE)%>%
  ggplot(aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+
  scale_fill_gradient2(low='steelblue1',high='tomato2',mid='white',
                       midpoint = 0,limit = c(-1,1), space = "Lab", name="Correlation")+
  coord_fixed()

heatmap_1 +
  geom_text(aes(Var1,Var2,label=value),color='black',size=4)+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal"
    
  )+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, 
                               title.position = "top", title.hjust = 0.5))




#Figure 3 Scatter Plots for the Top 2 Strong Correlations 
ggplot(Spotify, aes(energy, loudness)) +
  geom_point(alpha=0.1, color="sky blue") +
  geom_smooth(method = lm, color="tomato", se=TRUE)+
  scale_x_continuous(limits= c(0,1),expand = c(0.01, 0.01)) +
  labs(x = "Energy", y = "Loudness",title ="Loudness VS. Energy") +
  theme_classic()

ggplot(Spotify, aes(energy, acousticness)) +
  geom_point(alpha=0.1, color="#69b3a2") +
  geom_smooth(method = lm, color="tomato", se=TRUE)+
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  labs(x = "Energy", y = "Acousticness",title ="Acousticness VS. Energy") +
  theme_classic()


#Figure 4 Distribution of Popularity 
attach(Spotify)
hist(popularity,
     main='Distribution of Popularity',
     xlab='Popularity',
     ylab='Frequency',
     col='skyblue',
     xlim=c(0,100))
abline(v=1, col="tomato")
abline(v=26,col='tomato')
abline(v=42,col='tomato')
#lines(density(popularity),col='blue')


#Figure 5 Relationship Between Popularity and Other Attributes
df_2 <- Spotify %>% group_by(popularity) %>% 
  summarize(popularity = unique(popularity), acousticness = mean(acousticness), 
            danceability = mean(danceability), energy = mean(energy), liveness = mean(liveness), 
            loudness = mean(loudness),speechiness = mean(speechiness),valence = mean(valence)) 
df_2 <- data.frame(popularity = rownames(df_2),df_2) 
df_2 <- df_2 %>% select(-popularity.1)
df_2 <- df_2 %>% filter(popularity == 1 | popularity == 26|popularity == 51 |popularity == 76 |popularity == 98 )
df_2 <- reshape2::melt(df_2)
df_2
ggplot(df_2, aes(x = reorder(variable, value), y = value)) +
  geom_bar(aes(fill = value > 0), width=0.8, stat = "identity") +
  facet_wrap(~ popularity, nrow = 5, labeller = labeller(popularity = c("1" = "Popularity: 0",
                                                                        "26" = "Popularity: 25",
                                                                        "51" = "Popularity: 50",
                                                                        "76" = "Popularity: 75",
                                                                        "98" = "Popularity: 100"))) +
  coord_flip() +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA), legend.position="none") +
  labs(x = NULL, y = "Value",title='Popularity vs Music Attributes')+
  theme(plot.title = element_text(hjust = 0.5))


#Figure 6 The 50-year Trend of Track Attributes
Selected <- read.csv("D:/Information Visualization/Selected.csv")
str(Selected)
ggplot(Selected, aes(x=Year, y=Values, group=Attributes, color=Attributes)) +
  geom_line(size=1, linetype = "solid") +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  facet_grid(rows = vars(Attributes), scales = "free") +
  ggtitle("Time Analysis of Songs¡¯ Attribute") +
  ylab("Value of Each Attributes") +
  scale_x_continuous(limits= c(1970,2020),expand = c(0.01, 0.01)) +
  theme_bw()


#Figure 7 Number of Tracks Over the Year
library(readxl)
year <- read_excel("D:/Information Visualization/year.xlsx")

library(grDevices)
linechart1<-ggplot(year,aes(Year,Count))+
  geom_line()+
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Increase of Tracks Over the Year")+
  ylab('Number of Tracks Released')+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_fixed(0.0025)
linechart1


#Figure 8 The Most Appeared Artists on Last Decade¡¯s Billboard Chart
library(dplyr)
cloud <- Billboard %>% count(artist_1, sort = TRUE, )
cloud

library(RColorBrewer)
pal = brewer.pal(15,"Blues")

library(wordcloud)
wordcloud(words = cloud$artist_1, 
          freq = cloud$n, 
          scale = c(4,.5), 
          random.order = F,
          random.color = F,
          colors = pal) 

#Figure 9 Top Artists and the Number of Songs They Created
a <- read_csv("D:/Information Visualization/art.csv")
View(a)
c <- a[c(6,7,8,9,10),]
c
ggplot(c,aes(x=songs,y=artist))+
  geom_col(fill='tomato3')+
  labs(x = 'Number of Songs', y = "Artist",title='Top Artists vs Number of Songs Created')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=rank))


#Figure 10  Radar Chart of Singers¡¯ Track Attributes
#Radar
data <- data.frame(acousticness = c(10, 0,1.9,2.4,1.2),
                   danceability= c(10,0,6.7,6.4,6.6),
                   speechiness = c(10,0,2.1,0.9,0.5),
                   energy= c(10,0,5.8,5.9,7.2),
                   instrumentalness = c(10,0,2.7,0.7,0),
                   valence=c(10,0,3.9,4.7,5.6),
                   row.names = c("max", "min", "Drake", "Ariana Grande", 
                                 "Maroon 5"))

# Define fill colors
colors_fill <- c(scales::alpha("purple", 0.1),
                 scales::alpha("gold", 0.1),
                 scales::alpha("skyblue", 0.2))

# Define line colors
colors_line <- c(scales::alpha("purple", 0.9),
                 scales::alpha("gold", 0.9),
                 scales::alpha("royalblue", 0.9))

# Create plot
radarchart(data, 
           seg = 10,  # Number of axis segments
           title = "Attributes of Top 3 singers' songs",
           pcol = colors_line,
           pfcol = colors_fill,
           plwd = 5)

# Add a legend
legend(x=0.6, 
       y=1.35, 
       legend = rownames(data[-c(1,2),]), 
       bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)


#Figure 12 Bar Plot of Most Common Chords
library(forcats)
chords <- read_excel("D:/Information Visualization/most common chords.xlsx")

ggplot(chords,aes(x=reorder(chord,count), y=count)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()


#Figure 13 Treemap of Most Common Chords
library(treemap)
data <-read_excel("D:/Information Visualization/most common chords.xlsx")
treemap(data,
        index="chord",
        vSize="count",
        type="index")


#Figure 14 Circular Bar plot of Chord Progression
library(tidyverse)
progression <- read_excel("D:/Information Visualization/progression.xlsx")

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- progression
# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
number_of_bar
# I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
angle <-  90 - 360 * (label_data$rank-0.5) /number_of_bar 
# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$angle<-ifelse( angle < -90, 1, 0)
# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #
head(label_data)

p <- ggplot(progression, aes(x=reorder(progression,number), y=number)) +   
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,250)+
  
  # theme_minimal
  theme_minimal() +
  # Custom the theme: no axis title and no cartesian grid
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    # This remove unnecessary margin around plot
    plot.margin = unit(rep(-2,4), "cm"))+
  # This makes the coordinate polar instead of
  coord_polar(start = 0)+
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=progression, y=number+10, label=progression), 
            color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 