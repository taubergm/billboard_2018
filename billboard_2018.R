if (!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}
library("ggplot2")
if (!require(plyr)) {
  install.packages("plyr", repos="http://cran.us.r-project.org")
}
library(plyr)
if (!require(data.table)) {
  install.packages("data.table", repos="http://cran.us.r-project.org")
}
library(data.table)
if (!require(waffle)) {
  install.packages("waffle", repos="http://cran.us.r-project.org")
}
library(waffle)
if (!require(ggthemes)) {
  install.packages("ggthemes", repos="http://cran.us.r-project.org")
}
library(ggthemes)

workingDir = '/Users/michaeltauberg/projects/billboard'

# Popular Music in 2018 - Year in Review
csvName = "billboard_2018.csv"
data_name = "2018"

setwd(workingDir)

dt = read.csv(csvName)
#write.csv(dt_uniq, "songs_2018b.csv", row.names = FALSE)
dt_uniq = dt[!duplicated(dt[,c('title','artist')], fromLast=FALSE),] #fromlast to get highest value in "weeks_on_list" field
#dt_uniq = dt_uniq[order(dt_uniq$weeks, decreasing = TRUE),] 
dt_uniq = dt_uniq[order(dt_uniq$date),] 


dt$main_artist = dt$artist
dt$main_artist = tolower(dt$artist)
dt$main_artist = gsub(" featuring.*", "", dt$main_artist)
dt$main_artist = gsub(" with .*", "", dt$main_artist)
dt$main_artist = gsub(" & .*", "", dt$main_artist)
#dt$main_artist = gsub(" and .*", "", dt$main_artist)
dt$main_artist = gsub(" x .*", "", dt$main_artist)
dt$main_artist = gsub(", .*", "", dt$main_artist)
dt$main_artist = gsub(" duet.*", "", dt$main_artist)
dt$main_artist = gsub(" co-starring.*", "", dt$main_artist)
dt$main_artist = gsub("travi$", "travis", dt$main_artist)
dt$main_artist = gsub("jay z", "jay-z", dt$main_artist)
dt$main_artist = gsub("\\\"misdemeanor\\\"", "misdemeanor", dt$main_artist)
dt$main_artist = gsub(" + .*", "", dt$main_artist)
dt$main_artist = gsub("jay-z +.*", "", dt$main_artist)
dt$main_artist = gsub(" vs.*", "", dt$main_artist)


# count the number of artist rows
library(dplyr)
artists  = dt %>% 
  group_by(main_artist) %>%
  summarise(no_rows = length(main_artist))
artists = artists[order(artists$no_rows, decreasing = TRUE),] 
#top_artists = artists[0:20,]
data_name = "top_artists"
top_artists = read.csv("artists_2018.csv")
top_artists = top_artists[order(top_artists$no_rows, decreasing=TRUE),]
top_artists$main_artist = factor(top_artists$main_artist, levels = top_artists$main_artist[order(top_artists$no_rows, decreasing=TRUE)])
p = ggplot(top_artists, aes(x=main_artist, y=no_rows, fill=genre)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 Billboars Hot-100 Artists by weeks on the chart in 2018")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Artist") + ylab("Weeks on Billboard Hot-100 in 2018") 
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=10, height=7)

data_name = "top_artists_gender"
top_artists = read.csv("artists_2018.csv")
top_artists = top_artists[order(top_artists$no_rows, decreasing=TRUE),]
top_artists$main_artist = factor(top_artists$main_artist, levels = top_artists$main_artist[order(top_artists$no_rows, decreasing=TRUE)])
p = ggplot(top_artists, aes(x=main_artist, y=no_rows, fill=gender)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 Billboars Hot-100 Artists by weeks on the chart in 2018")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Artist") + ylab("Weeks on Billboard Hot-100 in 2018") 
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=9, height=7)


# popular music is a young persons game
# maybe do this with all artists
data_name = "top_artists_age"
km <- kmeans(top_artists$age,centers=3)
top_artists$cluster <- as.factor(km$cluster)
p = ggplot(top_artists, aes(x=age, fill="red", fill=cluster)) + geom_histogram(binwidth=1)
p = p + ggtitle("Age of Top 20 Artists in 2018")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p  + ylab("num artists in top 20") 
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=9, height=7)

# God might be a woman, but most top artists are not 
genders = c()
total = nrow(all_artists)
all_artists = read.csv("all_artists_2018.csv")
for (gender in levels(droplevels(all_artists$gender))) {
  gender_subset = all_artists[all_artists$gender==gender,]
  gender_total = nrow(gender_subset)
  stats_row = c(gender,gender_total,total,gender_total/total*100 )
  genders = rbind(genders, stats_row)
}
genders = as.data.table(genders)
genders = as.data.frame(genders)
colnames(genders) = c("genders","num_genders","total","percent_genders")
genders$percent_genders=as.numeric(as.character(genders$percent_genders))
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    axis.text.x=element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
p = ggplot(genders, aes(x="", y=percent_genders, fill=genders)) + geom_bar(width = 1, stat = "identity")
p = p + coord_polar("y", start=0) 
p = p + blank_theme 
p = p + ggtitle("Gender of Billboard Hot-100 Artists in 2018")
p = p + scale_fill_manual(values=c("pink","light blue","grey"))
ggsave(filename = "./genders_2018.png", plot=p, width=6, height=6) 

vals = as.numeric(as.character(genders$num_genders))
val_names  =  sprintf("%s (%s)", c("Female", "Male", "Mixed"), scales::percent(round(vals/sum(vals), 2)))
names(vals)  =  val_names

waffle::waffle(vals,
               colors = c("pink","light blue","grey"), 
               title = "Top 2018 Billboard Artists by Gender")
  ggthemes::scale_fill_tableau(name=NULL)



songs  = dt %>% 
  group_by(title) %>%
  summarise(no_rows = length(title))
songs = songs[order(songs$no_rows, decreasing = TRUE),] 
#top_songs = songs[0:20,]
top_songs = read.csv("top_songs_2018.csv")
data_name = "top_songs"
top_songs = top_songs[order(top_songs$no_rows, decreasing=TRUE),]
top_songs$title = factor(top_songs$title, levels = top_songs$title[order(top_songs$no_rows, decreasing=TRUE)])
p = ggplot(top_songs, aes(x=title, y=no_rows, fill=genre)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 Billboars Hot-100 Artists by weeks on the chart in 2018")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Song") + ylab("Weeks on Billboard Hot-100 in 2018") 
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=9, height=7)


hits = dt[dt$rank == 1,]
num_ones = hits %>% 
  group_by(title,artist) %>%
  summarise(no_rows = length(title))
num_ones = num_ones[order(num_ones$no_rows, decreasing = TRUE),] 
#hits = hits[!duplicated(hits[,c('title','artist')], fromLast=FALSE),]

data_name = "hits"
hits = read.csv("hits_2018.csv")
hits$title = factor(hits$title, levels = hits$title[order(hits$no_rows, decreasing=TRUE)])
p = ggplot(hits, aes(x=title, y=no_rows, fill=genre)) + geom_bar(stat="identity") 
p = p + ggtitle("#1 Hits Songs in 2018")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Song") + ylab("Weeks at #1") 
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=7, height=10)


genre_data = read.csv('billboard_songs_2018_genre_spotify_data.csv')
genres = c()
total = nrow(genre_data)
for (genre in levels(droplevels(genre_data$broad_genre))) {
  genre_subset = genre_data[genre_data$broad_genre==genre,]
  genre_total = nrow(genre_subset)
  stats_row = c(genre,genre_total,total,genre_total/total*100 )
  genres = rbind(genres, stats_row)
}
genres = as.data.table(genres)
genres = as.data.frame(genres)
colnames(genres) = c("genres","num_genres","total","percent_genres")
genres$percent_genres=as.numeric(as.character(genres$percent_genres))

vals = as.numeric(as.character(genres$num_genres))
val_names  =  sprintf("%s (%s)", c("country", "edm", "pop", "r&b", "rap", "rock", "unknown"), scales::percent(round(vals/sum(vals), 2)))
names(vals)  =  val_names

waffle::waffle(vals,
               size = 1, 
               #colors = c("pink","light blue","grey"), 
               title = "Billboard 2018 Songs by Genre")
ggthemes::scale_fill_tableau(name=NULL)
png('genre_waffle.png')
dev.off()


# fix cursing in
spotify_data = read.csv('billboard_songs_2018_genre_spotify_data.csv', na.strings = "unknown")
hist(spotify_data$duration_ms, na.rm = TRUE)
hist(spotify_data$valence, na.rm = TRUE)
hist(spotify_data$energy, na.rm = TRUE)
hist(spotify_data$danceability, na.rm = TRUE)
hist(spotify_data$loudness, na.rm = TRUE)
hist(spotify_data$tempo, na.rm = TRUE)

# age of top artists
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# plot bar chart of ages by genre
ages = c()
artists = read.csv("all_artists_2018_age_genre_gender2.csv", na.strings = "unknown" )
artists = artists[!is.na(artists$main_genre),]
artists$age = as.numeric(as.character(artists$age))
for (genre in levels(droplevels(artists$main_genre))) {
  print(genre)
  genre_subset = artists[artists$main_genre==genre,]
  median_age = median(genre_subset$age, na.rm = TRUE)
  mean_age = mean(genre_subset$age, na.rm = TRUE)
  stats_row = c(genre,median_age,mean_age)
  ages = rbind(ages, stats_row)
}
# plot the histogram of ages
rock = artists[artists$main_genre=="rock",]
p = ggplot(rock, aes(x=age, fill="red")) + geom_histogram(binwidth=1)
p = p + ggtitle("Age of Rock Artists in 2018")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=10), axis.title=element_text(size=10))
p = p  + ylab("num rock artists") + guides(fill=FALSE)
p1 = p
country = artists[artists$main_genre=="country",]
p = ggplot(country, aes(x=age, fill="red")) + geom_histogram(binwidth=1)
p = p + ggtitle("Age of Country Artists in 2018")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=10), axis.title=element_text(size=10))
p = p  + ylab("num country artists") + guides(fill=FALSE)
p2 = p
pop = artists[artists$main_genre=="pop",]
p = ggplot(pop, aes(x=age, fill="red")) + geom_histogram(binwidth=1)
p = p + ggtitle("Age of Pop Artists in 2018")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=10), axis.title=element_text(size=10))
p = p  + ylab("num pop artists") + guides(fill=FALSE)
p3 = p
rb = artists[artists$main_genre=="r&b",]
p = ggplot(rb, aes(x=age, fill="red")) + geom_histogram(binwidth=1)
p = p + ggtitle("Age of R&B Artists in 2018")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=10), axis.title=element_text(size=10))
p = p  + ylab("num r&b artists") + guides(fill=FALSE)
p4 = p
rap = artists[artists$main_genre=="rap",]
p = ggplot(rap, aes(x=age, fill="red")) + geom_histogram(binwidth=1)
p = p + ggtitle("Age of Rap Artists in 2018")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=10), axis.title=element_text(size=10))
p = p + ylab("num rap artists") + guides(fill=FALSE)
p5 = p
edm = artists[artists$main_genre=="edm",]
p = ggplot(edm, aes(x=age, fill="red")) + geom_histogram(binwidth=1)
p = p + ggtitle("Age of EDM Artists in 2018")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=10), axis.title=element_text(size=10))
p = p + ylab("num edm artists") + guides(fill=FALSE)
p6 = p

png("./multiplot.png", width=700, height=700)
p = multiplot(p1, p2, p3, p4, p5, p6, cols=2)
dev.off()


latin = read.csv("latin_2018.csv")
data_name = "latin"
latin = latin[order(latin$weeks, decreasing=TRUE),]
latin$artist = factor(latin$artist, levels = latin$artist[order(latin$weeks, decreasing=TRUE)])
p = ggplot(latin, aes(x=artist, y=weeks, fill="blue")) + geom_bar(stat="identity") 
p = p + ggtitle("Latin Artists on Billboard Hot-100 in 2018")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=15), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Artists") + ylab("Weeks on Billboard Hot-100 in 2018") 
p = p + guides(fill=FALSE)
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=9, height=7)


