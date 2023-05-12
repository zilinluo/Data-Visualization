library(readr)
library(forcats)
library(ggplot2)

library(reshape2)
library(dplyr)

library(gganimate)
library(babynames)
library(hrbrthemes)
s<- read_csv("/Users/rosalind/Desktop/Informational Visualizaiton/consultancy project/Report 3/Team 3-No Music No Life-Assignment 3/R/Spotify.csv")
View(s)
str(s)
summary(s)
#Figure2 Corrections of Music Attributes
#data preparation
attach(s)
df <- s[,c(4,5,7,9,11,12,15,16,17,6,14)]
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
  scale_fill_gradient2(low='grey40',high='forestgreen',mid='white',
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
#Figure 4 Histogram of Popularity
attach(s)
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

#Figure 5 Relationship between Popularity and Music Attributes
df_2 <- s %>% group_by(popularity) %>% 
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

#Figure 7 Line for the increase of tracks
library(grDevices)
library(readxl)
year <- read_excel("Desktop/Informational Visualizaiton/year.xlsx")
View(year)
linechart1<-ggplot(year,aes(Year,Count))+
  geom_line()+
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Increase of Tracks Over the Year")+
  ylab('Number of Tracks Released')+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_fixed(0.0025)
linechart1

linechart1<-ggplot(year,aes(Year,Count))+
  geom_line(color='green4',size=1)+

  ggtitle("Increase of Tracks Over the Year")+
  ylab('Number of Tracks Released')+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_fixed(0.0025)+
  geom_abline(aes(slope=0,intercept=2000),color='grey45') 

#Figure9 Most Popular Artists vs number of Songs Created
a <- read_csv("Desktop/art.csv")
View(a)
c <- a[c(6,7,8,9,10),]
c
ggplot(c,aes(x=songs,y=artist))+
  geom_col(fill='tomato3')+
  labs(x = 'Number of Songs', y = "Artist",title='Top Artists vs Number of Songs Created')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=rank))
  





