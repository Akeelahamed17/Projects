#####MA429 Group Summative Project#####

####installing and loading packages and functions####

# install.packages("rebus")
# install.packages("data.table")
# install.packages("stringr")
# install.packages("readr")
# install.packages("lattice")
# install.packages("caret")
# install.packages("magrittr")
# install.packages("dplyr")
# install.packages("corrplot")
# install.packages("Amelia")
# install.packages("e1071")
# install.packages("MASS")
# install.packages("class")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("kernlab")
# install.packages("randomForest")
# install.packages("nnet")
# install.packages("stats")

library(base) #base package, eg: Sys.time()
library(rebus) #PURPOSE?
library(data.table) #installed for using %like% 
library(stringr) #for functions like grep(),substr() etc
library(readr) #for reading .tsv files
library(lattice) #data visualisation
library(caret) #data visualisation
library(magrittr) #for piping commands %>%
library(dplyr) #advanced functions for dataframes
library(corrplot) #visualisation of correlation
library(Amelia) #visualising missing values
library(e1071) #check skewness of variables
library(MASS) #for Linear/Quadratic Discriminant Analysis, Box-Cox transformation
library(class) #for k-Nearest Neighbours
library(ggplot2) #data visualisation
library(gridExtra) #data visualisation
library(kernlab)   #support package for svm kernels
library(randomForest) #random forest 
library(nnet)   #multi-class logistic regression
library(stats) #basic functions


#function to find number of occurences of a particular character in a string
countCharOccurrences <- function(char, s) {
  s2 <- gsub(char,"",s)
  return (nchar(s) - nchar(s2))
}

####importing dataset####

#Note: some pre-processing was done in Excel:
#Removed the special characters from the movie titles 

IMDB_raw<-read.csv("movie_metadata.csv",na.strings=c("","NA"))
#glance over the dataset
dim(IMDB_raw)
colnames(IMDB_raw)
sapply(IMDB_raw, class)

####data preprocessing####

##initial steps##

IMDB = IMDB_raw

#adding identifier: an attribute that uniquely determines every instance
#tconst is part of each unique movie link
IMDB$tconst = sapply(IMDB$movie_imdb_link, function(x) substr(x,27,35))

#import additional datasets for completing missing values
#https://www.imdb.com/interfaces/
#only relevant entries from the imported datasets are kept, i.e. only relevant for the movies under consideration
IMDB_extra = NULL
imdb_title_basics = read_tsv(file="IMDB/title_basics.tsv", na="\\N")
IMDB_extra = imdb_title_basics %>% filter(tconst %in% IMDB$tconst) %>% dpylr::select(tconst, titleType, primaryTitle, startYear, runtimeMinutes, genres)
rm(imdb_title_basics)
imdb_title_crew = read_tsv(file="IMDB/title_crew.tsv", na="\\N")
tmp = imdb_title_crew %>% filter(tconst %in% IMDB$tconst) %>% dpylr::select(tconst, directors)
IMDB_extra = merge(IMDB_extra, tmp, by="tconst", all=TRUE)
rm(imdb_title_crew)
tmp = NULL
imdb_title_ratings = read_tsv(file="IMDB/title_ratings.tsv", na="\\N")
tmp = imdb_title_ratings %>% filter(tconst %in% IMDB$tconst)
IMDB_extra = merge(IMDB_extra, tmp, by="tconst", all=TRUE)
rm(imdb_title_ratings, tmp)
#not all movies under consideration could be found in the additional datasets

IMDB_extra_crew = NULL
imdb_title_principles = read_tsv(file="IMDB/title_principles.tsv", na="\\N")
IMDB_extra_crew = imdb_title_principles %>% filter(tconst %in% IMDB$tconst) %>% dpylr::select(tconst, ordering, nconst, category)
rm(imdb_title_principles)

IMDB_extra_ppl = NULL
imdb_name_basics = read_tsv(file="IMDB/name_basics.tsv", na="\\N")
IMDB_extra_ppl = imdb_name_basics %>% filter(nconst %in% IMDB_extra_crew$nconst) %>% dpylr::select(nconst, primaryName)
rm(imdb_name_basics)

##overview of missing values##

paste("Number of incomplete observations:",sum(!complete.cases(IMDB)))

#visualising missing values
missmap(IMDB, col=c("red", "grey"), legend=TRUE,y.labels = rep("",dim(IMDB)[1]))
#finding number of missing values for each feature
colSums(sapply(IMDB, is.na))

##looking for duplicated movies##

#identify entries by the unique IMDB tconst
paste("Number of duplicate entries: ", sum(duplicated(IMDB$tconst)))
#have a look at the duplicates
dupl<-data.frame(which(duplicated(IMDB$tconst)), IMDB$movie_title[which(duplicated(IMDB$tconst))])
names(dupl) = c("row", "movie title")
dupl
rm(dupl)
#all duplicates are deleted (the first appearance in the dataset is kept)
paste("Observations before duplicate deletion: ",dim(IMDB)[1])
IMDB <- IMDB[!duplicated(IMDB$tconst), ]
paste("Observations after duplicate deletion: ",dim(IMDB)[1])

##creating binned scores##
#two different strategies are applied:
#binned_score1 divides the scores into intuitive bins reflecting levels of popularity 
IMDB$binned_score1 <- cut(IMDB$imdb_score, breaks = c(0,4,5,5.5,6,6.5,7,7.5,8,10))
cbind(freq=table(IMDB$binned_score1), percentage=prop.table(table(IMDB$binned_score1))*100)
summary(IMDB$binned_score1)
histogram(IMDB$binned_score1)
#binned_score2 divides the scores into rough bins used in past work on the same dataset by data scientists 
IMDB$binned_score2 <- cut(IMDB$imdb_score, breaks = c(0,4,6,8,10))
cbind(freq=table(IMDB$binned_score2), percentage=prop.table(table(IMDB$binned_score2))*100)
summary(IMDB$binned_score2)
histogram(IMDB$binned_score2)
#binned scores are necessary for the way we deal with the missing values in gross and budget features

##dealing with missing values and other feature specific transformations##

#genres feature

#missing values
sum(sapply(IMDB$genres, is.na))

#genres can combine several single genres in one cell, each unique genre is added as a binary feature to the dataset
# create a new data frame
genres.df <- as.data.frame(IMDB[,c("genres", "tconst")])
IMDB=subset(IMDB,select = -c(genres))
# separate different (unique) genres into new columns
genres.df$Action <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Action") 1 else 0)
genres.df$Adventure <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Adventure") 1 else 0)
genres.df$Animation <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Animation") 1 else 0)
genres.df$Biography <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Biography") 1 else 0)
genres.df$Comedy <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Comedy") 1 else 0)
genres.df$Crime <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Crime") 1 else 0)
genres.df$Documentary <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Documentary") 1 else 0)
genres.df$Drama <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Drama") 1 else 0)
genres.df$Family <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Family") 1 else 0)
genres.df$Fantasy <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Fantasy") 1 else 0)
genres.df$`Film-Noir` <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Film-Noir") 1 else 0)
genres.df$History <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "History") 1 else 0)
genres.df$Horror <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Horror") 1 else 0)
# the commented code handles music and musical as different categories
# genres.df$Music <- sapply(1:length(genres.df$genres), function(x) str_detect(genres.df[x,1], "Music"))
# genres.df$Musical <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Musical") 1 else 0)
# genres.df$Music<-genres.df$Music - genres.df$Musical
# instead, the following line of code combines the two categories
genres.df$Music <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Music") 1 else 0)
genres.df$Mystery <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Mystery") 1 else 0)
genres.df$News <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "News") 1 else 0)
genres.df$Romance <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Romance") 1 else 0)
genres.df$`Reality-TV` <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Reality-TV") 1 else 0)
genres.df$`Sci-Fi` <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Sci-Fi") 1 else 0)
genres.df$Short <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Short") 1 else 0)
genres.df$Sport <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Sport") 1 else 0)
genres.df$Thriller <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Thriller") 1 else 0)
genres.df$War <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "War") 1 else 0)
genres.df$Western <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Western") 1 else 0)
genres.df$`Game-Show` <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Game-Show") 1 else 0)
# have a look at the distribution of genres         
genre.sum = colSums(genres.df[,-c(1:2)])
barplot(genre.sum)
rm(genre.sum)
# merging back to IMDB dataset
genres.df = genres.df[,-1]
IMDB=merge(x=IMDB, y=genres.df, by="tconst", all.x=TRUE)
# remove those columns which have very low occurences again
IMDB = subset(IMDB, select=-c(`Film-Noir`,News,`Reality-TV`,Short,`Game-Show`))

#gross feature

#missing values
sum(sapply(IMDB$gross, is.na))
length(which(IMDB$gross==0))

#pre-check: are there any observations where both gross and budget are missing?
length(which(is.na(IMDB[,"budget"] && is.na(IMDB[,"gross"]))))

#to deal with missing values we recommend assigning the gross average of the movies
#which are in the same category i.e. have the same genre (share at least 2 genres)
#and are in the same IMDB score bin
# first, convert all NAs to 0s
index<-which(is.na(IMDB[,"gross"]))
IMDB[index,"gross"]=0
# then, assign average values as explained above
for(i in index){
  index2<-NULL
  for(k in 1:dim(genres.df)[1]){
    feature_sum=0
    for(j in 2:dim(genres.df)[2]){
      feature_sum=feature_sum + if(genres.df[k,j]==genres.df[i,j]) 1 else 0
    }
    if(feature_sum>=2 && IMDB$binned_score1[i]==IMDB$binned_score1[k]) index2<-append(index2,k)
  }
  IMDB[i,"gross"]=mean(IMDB[index2,"gross"])
}
rm(i, index, index2, k, j, feature_sum)
length(which(IMDB$gross==0))
#no 0s left, i.e. all movies with missing gross were assigned an average gross from similar movies

#budget feature

#missing values
sum(sapply(IMDB$budget, is.na))
length(which(IMDB$budget==0))

#to deal with missing values we recommend assigning the budget average of the movies
#which are in the same category i.e. have the same genre (share at least 2 genres)
#and are in the same IMDB score bin
# first, convert all NAs to 0s
index<-which(is.na(IMDB[,"budget"]))
IMDB[index,"budget"]=0
# then, assign average values as explained above
for(i in index){
  index2<-NULL
  for(k in 1:dim(genres.df)[1]){
    feature_sum=0
    for(j in 2:dim(genres.df)[2]){
      feature_sum=feature_sum + if(genres.df[k,j]==genres.df[i,j])1 else 0
    }
    if(feature_sum>=2 && IMDB$binned_score1[i]==IMDB$binned_score1[k]) index2<-append(index2,k)
  }
  IMDB[i,"budget"]=mean(IMDB[index2,"budget"])
}
rm(i, index, index2, k, j, feature_sum)
length(which(IMDB$budget==0))
#no 0s left, i.e. all movies with missing budget were assigned an average budget from similar movies

#since we don't need genres.df anymore, we can drop it
rm(genres.df)

#copy of IMDB
#IMDB_backup = IMDB

#director_name feature

#missing values
sum(sapply(IMDB$director_name, is.na))

#to deal with missing values we first look if we can find some of the directors in the additional datasets
# first, we have a look at the movies which don't have a director assigned
tmp=IMDB %>% filter(is.na(director_name)) %>% dyplr::select(movie_title, num_voted_users, budget, title_year, imdb_score, tconst)
tmp
mean(tmp$imdb_score)
mean(IMDB$imdb_score)
# then, we search and add director names if we find them in the additional datasets
tmp2=IMDB_extra %>% filter(tconst %in% tmp$tconst) %>% dyplr::select(tconst, titleType, directors)
tmp2$ndir=sapply(tmp2$directors, function(x) 1+countCharOccurrences(",",x))
tmp = merge(tmp, tmp2, by="tconst", all=TRUE)
rm(tmp2)
colnames(tmp)[8] = "nconst"
tmp$nconst
#the additional data we found shows that most of the movies which do not have a director assigned in fact have multiple directors
#(e.g. series where every episode has a different director)
tmp = tmp %>% filter(ndir==1) %>% dyplr::select(tconst, nconst) %>% merge(IMDB_extra_ppl, by="nconst", all.x=TRUE) %>% dyplr::select(tconst, primaryName)
IMDB = merge(IMDB, tmp, by="tconst", all.x=TRUE)
IMDB$director_name = as.character(IMDB$director_name)
IMDB$director_name[is.na(IMDB$director_name)] = IMDB$primaryName[is.na(IMDB$director_name)]
rm(tmp)
IMDB = subset(IMDB, select=-c(primaryName))
sum(is.na(IMDB$director_name))
#we could only add a few more directors since most of the other movies with missing director name have multiple as explained above

#the feature is replaced by another one which seems to be more promising:
#the number of different movies in the data set which were directed by the same director
# first, we have a closer look at the director_name feature
length(unique(IMDB$director_name))
# On an average, how many movies has a director directed?
length(IMDB$director_name)/length(unique(IMDB$director_name))
# number of movies in the dataset directed by each director
barplot(table(IMDB$director_name), ylim=c(0,30))
# then, we conduct feature transformation to number of movies of a director (in the dataset)
dirs = data.frame(IMDB$director_name, IMDB$imdb_score)
colnames(dirs) = c("director_name", "imdb_score")
dir_dist = table(dirs$director_name)
table(dir_dist)
dirs$dir_app = dir_dist[dirs$director_name]
dirs$dir_app = as.numeric(dirs$dir_app)
rm(dir_dist)
# movies which have a missing value (as explained above) are assigned 1
dirs$dir_app[is.na(dirs$dir_app)] = 1
max(dirs$dir_app)
# merging back to IMDB dataset
IMDB$dir_app = dirs$dir_app
rm(dirs)
# remove the director_name feature
IMDB<- subset(IMDB, select=-c(director_name))

#actor_1_name feature

#missing values
sum(sapply(IMDB$actor_1_name, is.na))

#the feature is replaced by another one which seems to be more promising:
#the number of different movies in the data set in which the actor participated as actor_1
# first, we have a closer look at the actor_1_name feature
length(unique(IMDB$actor_1_name))
# On an average, in how many movies has an actor acted as actor_1?
length(IMDB$actor_1_name)/length(unique(IMDB$actor_1_name))
# number of movies in the dataset each actor acted in as actor_1
barplot(table(IMDB$actor_1_name), ylim=c(0,30))
# then, we conduct feature transformation to number of movies of an actor (in the dataset) as actor_1
dirs = data.frame(IMDB$actor_1_name, IMDB$imdb_score)
colnames(dirs) = c("actor_1_name", "imdb_score")
dir_dist = table(dirs$actor_1_name)
table(dir_dist)
dirs$dir_app = dir_dist[dirs$actor_1_name]
dirs$dir_app = as.numeric(dirs$dir_app)
rm(dir_dist)
# few movies which have a missing value are assigned 1
dirs$dir_app[is.na(dirs$dir_app)] = 1
max(dirs$dir_app)
# merging back to IMDB dataset
IMDB$actor_1_app = dirs$dir_app
rm(dirs)
# remove the actor_1_name feature
IMDB<- subset(IMDB, select=-c(actor_1_name))

#actor_2_name feature

#missing values
sum(sapply(IMDB$actor_2_name, is.na))

#the feature is replaced by another one which seems to be more promising:
#the number of different movies in the data set in which the actor participated as actor_2
# first, we have a closer look at the actor_2_name feature
length(unique(IMDB$actor_2_name))
# On an average, in how many movies has an actor acted as actor_2?
length(IMDB$actor_2_name)/length(unique(IMDB$actor_2_name))
# number of movies in the dataset each actor acted in as actor_2
barplot(table(IMDB$actor_2_name), ylim=c(0,30))
# then, we conduct feature transformation to number of movies of an actor (in the dataset) as actor_2
dirs = data.frame(IMDB$actor_2_name, IMDB$imdb_score)
colnames(dirs) = c("actor_2_name", "imdb_score")
dir_dist = table(dirs$actor_2_name)
table(dir_dist)
dirs$dir_app = dir_dist[dirs$actor_2_name]
dirs$dir_app = as.numeric(dirs$dir_app)
rm(dir_dist)
# few movies which have a missing value are assigned 1
dirs$dir_app[is.na(dirs$dir_app)] = 1
max(dirs$dir_app)
# merging back to IMDB dataset
IMDB$actor_2_app = dirs$dir_app
rm(dirs)
# remove the actor_2_name feature
IMDB<- subset(IMDB, select=-c(actor_2_name))

#actor_3_name feature

#missing values
sum(sapply(IMDB$actor_3_name, is.na))

#the feature is replaced by another one which seems to be more promising:
#the number of different movies in the data set in which the actor participated as actor_3
# first, we have a closer look at the actor_3_name feature
length(unique(IMDB$actor_3_name))
# On an average, in how many movies has an actor acted as actor_3?
length(IMDB$actor_3_name)/length(unique(IMDB$actor_3_name))
# number of movies in the dataset each actor acted in as actor_3
barplot(table(IMDB$actor_3_name), ylim=c(0,30))
# then, we conduct feature transformation to number of movies of an actor (in the dataset) as actor_3
dirs = data.frame(IMDB$actor_3_name, IMDB$imdb_score)
colnames(dirs) = c("actor_3_name", "imdb_score")
dir_dist = table(dirs$actor_3_name)
table(dir_dist)
dirs$dir_app = dir_dist[dirs$actor_3_name]
dirs$dir_app = as.numeric(dirs$dir_app)
rm(dir_dist)
# few movies which have a missing value are assigned 1
dirs$dir_app[is.na(dirs$dir_app)] = 1
max(dirs$dir_app)
# merging back to IMDB dataset
IMDB$actor_3_app = dirs$dir_app
rm(dirs)
# remove the actor_3_name feature
IMDB<- subset(IMDB, select=-c(actor_3_name))

#plot_keywords feature

#too many unique categories, therefore the attribute is dropped
IMDB<- subset(IMDB, select=-c(plot_keywords))

#director_facebook_likes feature

#missing values (0s are considered as missing values as well)
sum(is.na(IMDB$director_facebook_likes))
length(which(IMDB$director_facebook_likes==0))

#to deal with missing values we recommend assigning the facebook likes average of the directors
index = which(IMDB$director_facebook_likes==0 | is.na(IMDB$director_facebook_likes))
tmp = round(mean(IMDB$director_facebook_likes[-index]))
tmp
IMDB$director_facebook_likes[index] = tmp
length(which(IMDB$director_facebook_likes==0 | is.na(IMDB$director_facebook_likes)))
rm(index, tmp)

#actor_1_facebook_likes feature

#missing values (0s are considered as missing values as well)
sum(is.na(IMDB$actor_1_facebook_likes))
length(which(IMDB$actor_1_facebook_likes==0))

#to deal with missing values we recommend assigning the facebook likes average of the actors
index = which(is.na(IMDB$actor_1_facebook_likes) | IMDB$actor_1_facebook_likes==0)
tmp = round(mean(IMDB$actor_1_facebook_likes[-index]))
tmp
IMDB$actor_1_facebook_likes[index] = tmp
length(which(is.na(IMDB$actor_1_facebook_likes) | IMDB$actor_1_facebook_likes==0))
rm(index, tmp)

#actor_2_facebook_likes feature

#missing values (0s are considered as missing values as well)
sum(is.na(IMDB$actor_2_facebook_likes))
length(which(IMDB$actor_2_facebook_likes==0))

#to deal with missing values we recommend assigning the facebook likes average of the actors
index = which(is.na(IMDB$actor_2_facebook_likes) | IMDB$actor_2_facebook_likes==0)
tmp = round(mean(IMDB$actor_2_facebook_likes[-index]))
tmp
IMDB$actor_2_facebook_likes[index] = tmp
length(which(is.na(IMDB$actor_2_facebook_likes) | IMDB$actor_2_facebook_likes==0))
rm(index, tmp)

#actor_3_facebook_likes feature

#missing values (0s are considered as missing values as well)
sum(is.na(IMDB$actor_3_facebook_likes))
length(which(IMDB$actor_3_facebook_likes==0))

#to deal with missing values we recommend assigning the facebook likes average of the actors
index = which(is.na(IMDB$actor_3_facebook_likes) | IMDB$actor_3_facebook_likes==0)
tmp = round(mean(IMDB$actor_3_facebook_likes[-index]))
tmp
IMDB$actor_3_facebook_likes[index] = tmp
length(which(is.na(IMDB$actor_3_facebook_likes) | IMDB$actor_3_facebook_likes==0))
rm(index, tmp)

#cast_total_facebook_likes feature

#missing values (0s are considered as missing values as well)
sum(is.na(IMDB$cast_total_facebook_likes))
length(which(IMDB$cast_total_facebook_likes==0))

#to deal with missing values we assigned the sum of the actor facebook likes
IMDB %>% filter(IMDB$cast_total_facebook_likes==0) %>% dyplr::select(movie_title, actor_1_facebook_likes)
index = which(IMDB$cast_total_facebook_likes==0)
IMDB$cast_total_facebook_likes[index] = IMDB$actor_1_facebook_likes[index]+IMDB$actor_2_facebook_likes[index]+IMDB$actor_3_facebook_likes[index]
rm(index)

#movie_facebook_likes feature

#missing values (0s are considered as missing values as well)
sum(is.na(IMDB$movie_facebook_likes))
length(which(IMDB$movie_facebook_likes==0))

#to deal with missing values we recommend assigning the facebook likes average of the movies
index = which(IMDB$movie_facebook_likes==0)
tmp = round(mean(IMDB$movie_facebook_likes[-index]))
tmp
IMDB$movie_facebook_likes[index] = tmp
length(which(IMDB$movie_facebook_likes==0 | is.na(IMDB$movie_facebook_likes)))
rm(index, tmp)

#duration feature

#missing values
sum(sapply(IMDB$duration, is.na))
length(which(IMDB$duration==0))

#to deal with missing values we checked those movies for their durations manually, if not available the duration mean was assigned
nas = data.frame(which(is.na(IMDB$duration)), IMDB[which(is.na(IMDB$duration)),9], IMDB[which(is.na(IMDB$duration)),19])
names(nas) = c("row", "movie title", "year")
nas
rm(nas)
IMDB[which(is.na(IMDB$duration)),4] = c(136,130,146,100,379,NA,105,153,105,144,99,143,NA,NA,90)
IMDB$duration[is.na(IMDB$duration)] <- round(mean(IMDB$duration, na.rm = TRUE))
sum(sapply(IMDB$duration, is.na))

#language feature

#missing values
sum(sapply(IMDB$language, is.na))

#most of the movies are in English
table(IMDB$language)
length(IMDB$language[IMDB$language=="English"])/length(IMDB$language)
mean(na.omit(IMDB$imdb_score[IMDB$language=="English"]))
mean(na.omit(IMDB$imdb_score[IMDB$language!="English"]))
#this feature is closely related to the country feature and is considered further below

#country feature

#missing values
sum(sapply(IMDB$country, is.na))

#the few observations with missing values are dropped
IMDB = IMDB[!is.na(IMDB$country),]

#most of the movies are from the US, second most from UK
table(IMDB$country)
length(IMDB$country[IMDB$country=="USA"])/length(IMDB$country)
mean(IMDB$imdb_score[IMDB$country=="USA"])
mean(IMDB$imdb_score[IMDB$country=="UK"])
mean(IMDB$imdb_score[IMDB$country!="USA" & IMDB$country!="UK"])
#all non US and non UK movies are grouped into "Other"
IMDB$country = as.character(IMDB$country)
IMDB$country[IMDB$country!="USA" & IMDB$country!="UK"] = "Other"

#there are only a few movies which are from the US but not English
length(which(IMDB$language!="English" & IMDB$country=="USA"))
#because of this close relationship of English movies and English speaking countries, language feature is dropped
IMDB<- subset(IMDB, select=-c(language))

#aspect_ratio feature

#missing values
sum(is.na(IMDB$aspect_ratio))

#most of the aspect ratios are from 2.35, second most are 1.85
IMDB$aspect_ratio[is.na(IMDB$aspect_ratio)] <- 0
table(IMDB$aspect_ratio)
mean(IMDB$imdb_score[IMDB$aspect_ratio == 1.85])
mean(IMDB$imdb_score[IMDB$aspect_ratio == 2.35])
mean(IMDB$imdb_score[IMDB$aspect_ratio != 1.85 & IMDB$aspect_ratio != 2.35])
#all movies which don't have ratio 1.85 or 2.35 are grouped into "Other"
index = which(IMDB$aspect_ratio != 1.85 & IMDB$aspect_ratio != 2.35)
IMDB$aspect_ratio = as.character(IMDB$aspect_ratio)
IMDB$aspect_ratio[index] = "Other"

#title_year feature

#missing values
sum(is.na(IMDB$title_year))

#most missing values are found in the additional datasets
tmp=IMDB %>% filter(is.na(title_year)) %>% dyplr::select(tconst)
tmp2=select(IMDB_extra, tconst, startYear)
tmp=merge(x=tmp, y=tmp2, by="tconst", all.x=TRUE)
rm(tmp2)
IMDB = merge(IMDB, tmp, by="tconst", all.x=TRUE)
IMDB$title_year[is.na(IMDB$title_year)] = IMDB$startYear[is.na(IMDB$title_year)]
rm(tmp)
IMDB = subset(IMDB, select=-c(startYear))
#remaining obersvations with missing values are dropped
sum(is.na(IMDB$title_year))
IMDB = IMDB[!is.na(IMDB$title_year),]

#color feature

#missing values
sum(is.na(IMDB$color))

#missing values were checked manually and added
index<-which(is.na(IMDB[,"color"]))
IMDB[index,c("color","movie_title")]
# we checked for whether the movie was made in Black and white or color from the IMDB link given in the data set
# finding: all the movies with missing values were made in color 
IMDB[index,"color"]="Color"

#content_rating feature

#missing values
sum(is.na(IMDB$content_rating))

#missing values are dropped as they cannot be assigned to any category
IMDB <- IMDB[!is.na(IMDB$content_rating),]

#some of the smaller content rating categories are included in more common ones
table(IMDB$content_rating)
IMDB$content_rating[IMDB$content_rating == 'TV-PG']   <- 'PG' 
IMDB$content_rating[IMDB$content_rating == 'M']   <- 'PG' 
IMDB$content_rating[IMDB$content_rating == 'GP']  <- 'PG' 
IMDB$content_rating[IMDB$content_rating == 'X']   <- 'R'
IMDB$content_rating[IMDB$content_rating == 'NC-17']   <- 'R'
IMDB$content_rating[IMDB$content_rating == 'TV-MA']   <- 'R'
IMDB$content_rating[IMDB$content_rating == 'Approved']  <- 'R'
IMDB$content_rating[IMDB$content_rating == 'Not Rated'] <- 'R'
IMDB$content_rating[IMDB$content_rating == 'Passed']    <- 'R'
IMDB$content_rating[IMDB$content_rating == 'Unrated']   <- 'R'
IMDB$content_rating[IMDB$content_rating == 'TV-G']   <- 'G'
IMDB$content_rating[IMDB$content_rating == 'TV-14']   <- 'PG-13'
#remove remaining categories with very few observations
IMDB = IMDB[-which(IMDB$content_rating=="TV-Y"|IMDB$content_rating=="TV-Y7"),]

#facenumber_in_poster feature

#missing values
sum(is.na(IMDB$facenumber_in_poster))

# for data completion figures are taken from posters on IMDB
index<-which(is.na(IMDB[,"facenumber_in_poster"]))
IMDB[index,c("facenumber_in_poster","movie_title","movie_imdb_link")]
faces<-c(1,2,4,4,1,8,1,6,0,4,1,2)
IMDB[index,"facenumber_in_poster"]=faces
IMDB[index,c("facenumber_in_poster","movie_title")]
rm(faces,index)

#num_critic_for_reviews feature

#missing values
sum(is.na(IMDB$num_critic_for_reviews))

#since there are only a handful of observations where these values are missing, we drop these observations
index<-which(is.na(IMDB[,"num_critic_for_reviews"]))
IMDB<-IMDB[-index,]
rm(index)

#num_user_for_reviews feature

#missing values
sum(is.na(IMDB$num_user_for_reviews))

#num_voted_users feature

#missing values
sum(is.na(IMDB$num_voted_users))

#inorder to remove bias i.e. the score should be a good representative of the population,
#we include this measure where we only consider movies with votes more than 500
length(which(!IMDB$num_voted_users>=500))
IMDB = IMDB[IMDB$num_voted_users>=500,]

#any missing values left?
colSums(sapply(IMDB, is.na))

#remove tconst, movie_title and movie_imdb_link
IMDB = subset(IMDB, select=-c(tconst, movie_title,movie_imdb_link))

##make feature names "R conform"
colnames(IMDB)[39] = "Sci_Fi"

##variable convertion##

sapply(IMDB, class)
numeric_var = c("num_critic_for_reviews","duration","director_facebook_likes","actor_3_facebook_likes",
                "actor_1_facebook_likes","gross","num_voted_users","cast_total_facebook_likes","facenumber_in_poster",
                "num_user_for_reviews","budget","title_year","actor_2_facebook_likes","movie_facebook_likes",
                "dir_app","actor_1_app","actor_2_app","actor_3_app")
numeric_score = "imdb_score"
factor_var = c("color","country","content_rating","aspect_ratio","Action","Adventure",
               "Animation","Biography","Comedy","Crime","Documentary","Drama","Family","Fantasy","History",
               "Horror","Music","Mystery","Romance","Sci_Fi","Sport","Thriller","War","Western")
factor_score = c("binned_score1","binned_score2")
num = IMDB[,c(numeric_var, numeric_score)]
num = mutate_all(num, funs(as.numeric))
sapply(num, class)
fac = IMDB[,c(factor_var, factor_score)]
fac = mutate_all(fac, funs(as.factor))
sapply(fac, class)

IMDB = cbind(num, fac)
rm(num, fac)

##prepare factor levels

#remove unnecessary factor levels
for(i in factor_var){
  IMDB[[i]] = factor(IMDB[[i]])
}
rm(i)

##sknewness and log-transformations##

#check for skewness of numerical variables
histogram(IMDB$num_critic_for_reviews)
histogram(IMDB$duration)
histogram(IMDB$director_facebook_likes)
histogram(IMDB$actor_3_facebook_likes)
histogram(IMDB$actor_1_facebook_likes)
histogram(IMDB$gross)
histogram(IMDB$num_voted_users)
histogram(IMDB$cast_total_facebook_likes)
histogram(IMDB$facenumber_in_poster)
histogram(IMDB$num_user_for_reviews)
histogram(IMDB$budget)
histogram(IMDB$title_year)
histogram(IMDB$actor_2_facebook_likes)
histogram(IMDB$movie_facebook_likes)
histogram(IMDB$dir_app)
histogram(IMDB$actor_1_app)
histogram(IMDB$actor_2_app)
histogram(IMDB$actor_3_app)
#all of the numeric variables are skewed
skewness(IMDB$num_critic_for_reviews)
skewness(IMDB$duration)
skewness(IMDB$director_facebook_likes)
skewness(IMDB$actor_3_facebook_likes)
skewness(IMDB$actor_1_facebook_likes)
skewness(IMDB$gross)
skewness(IMDB$num_voted_users)
skewness(IMDB$cast_total_facebook_likes)
skewness(IMDB$facenumber_in_poster)
skewness(IMDB$num_user_for_reviews)
skewness(IMDB$budget)
skewness(IMDB$title_year)
skewness(IMDB$actor_2_facebook_likes)
skewness(IMDB$movie_facebook_likes)
skewness(IMDB$dir_app)
skewness(IMDB$actor_1_app)
skewness(IMDB$actor_2_app)
skewness(IMDB$actor_3_app)
#take log
IMDBtrans<-IMDB
IMDBtrans$num_critic_for_reviews=log(IMDB$num_critic_for_reviews+1)
IMDBtrans$duration = log(IMDB$duration+1)
IMDBtrans$director_facebook_likes = log(IMDB$director_facebook_likes+1)
IMDBtrans$actor_3_facebook_likes = log(IMDB$actor_3_facebook_likes+1)
IMDBtrans$actor_1_facebook_likes = log(IMDB$actor_1_facebook_likes+1)
IMDBtrans$gross = log(IMDB$gross+1)
IMDBtrans$num_voted_users = log(IMDB$num_voted_users+1)
IMDBtrans$cast_total_facebook_likes = log(IMDB$cast_total_facebook_likes+1)
IMDBtrans$facenumber_in_poster = log(IMDB$facenumber_in_poster+1)
IMDBtrans$num_user_for_reviews = log(IMDB$num_user_for_reviews+1)
IMDBtrans$budget = log(IMDB$budget+1)
IMDBtrans$title_year = log(IMDB$title_year+1)
IMDBtrans$actor_2_facebook_likes = log(IMDB$actor_2_facebook_likes+1)
IMDBtrans$movie_facebook_likes = log(IMDB$movie_facebook_likes+1)
IMDBtrans$dir_app = log(IMDB$dir_app+1)
IMDBtrans$actor_1_app = log(IMDB$actor_1_app+1)
IMDBtrans$actor_2_app = log(IMDB$actor_2_app+1)
IMDBtrans$actor_3_app = log(IMDB$actor_3_app+1)
#check for skewness again
histogram(IMDBtrans$num_critic_for_reviews)
histogram(IMDBtrans$duration)
histogram(IMDBtrans$director_facebook_likes)
histogram(IMDBtrans$actor_3_facebook_likes)
histogram(IMDBtrans$actor_1_facebook_likes)
histogram(IMDBtrans$gross)
histogram(IMDBtrans$num_voted_users)
histogram(IMDBtrans$cast_total_facebook_likes)
histogram(IMDBtrans$facenumber_in_poster)
histogram(IMDBtrans$num_user_for_reviews)
histogram(IMDBtrans$budget)
histogram(IMDBtrans$title_year)
histogram(IMDBtrans$actor_2_facebook_likes)
histogram(IMDBtrans$movie_facebook_likes)
histogram(IMDBtrans$dir_app)
histogram(IMDBtrans$actor_1_app)
histogram(IMDBtrans$actor_2_app)
histogram(IMDBtrans$actor_3_app)
#less skewness for all variables

##normalising the numeric variables##

#log normal dataset
IMDBtrans$num_critic_for_reviews=scale(IMDBtrans$num_critic_for_reviews)
IMDBtrans$duration = scale(IMDBtrans$duration)
IMDBtrans$director_facebook_likes = scale(IMDBtrans$director_facebook_likes)
IMDBtrans$actor_3_facebook_likes = scale(IMDBtrans$actor_3_facebook_likes)
IMDBtrans$actor_1_facebook_likes = scale(IMDBtrans$actor_1_facebook_likes)
IMDBtrans$gross = scale(IMDBtrans$gross)
IMDBtrans$num_voted_users = scale(IMDBtrans$num_voted_users)
IMDBtrans$cast_total_facebook_likes = scale(IMDBtrans$cast_total_facebook_likes)
IMDBtrans$facenumber_in_poster = scale(IMDBtrans$facenumber_in_poster)
IMDBtrans$num_user_for_reviews = scale(IMDBtrans$num_user_for_reviews)
IMDBtrans$budget = scale(IMDBtrans$budget)
IMDBtrans$title_year = scale(IMDBtrans$title_year)
IMDBtrans$actor_2_facebook_likes = scale(IMDBtrans$actor_2_facebook_likes)
IMDBtrans$movie_facebook_likes = scale(IMDBtrans$movie_facebook_likes)
IMDBtrans$dir_app = scale(IMDBtrans$dir_app)
IMDBtrans$actor_1_app = scale(IMDBtrans$actor_1_app)
IMDBtrans$actor_2_app = scale(IMDBtrans$actor_2_app)
IMDBtrans$actor_3_app = scale(IMDBtrans$actor_3_app)

####splitting into training and test data####

#create a training and a testing set with a 0.8:0.2 ratio by random sampling
#for both, the IMDB set as well as the transformed IMDB set
set.seed(1)
train <- sample(seq_len(nrow(IMDB)), size = floor(0.8*nrow(IMDB)))
train.data = IMDB[train,]
test.data = IMDB[-train,]
train.data.trans = IMDBtrans[train,]
test.data.trans = IMDBtrans[-train,]

####exploratory analysis####

#on training data only

##dataset overview##
dim(train.data)
colnames(train.data)
#be aware that there are still three features for scores: imdb_score, binned_score1 and binned_score2
sapply(train.data, class)
summary(train.data)

##IMDb score features##

#imdb_score
boxplot(train.data$imdb_score, xlab="", ylab="IMDb score")
par(mfrow=c(1,3))
plot(density(train.data$imdb_score), main="", xlab="IMDb score", ylab="Probability")

#binned_score1
cbind(freq=table(train.data$binned_score1), percentage=prop.table(table(IMDB$binned_score1))*100)
barplot(table(train.data$binned_score1), xlab="Score bins", ylab="Occurences")

#binned_score2
cbind(freq=table(train.data$binned_score2), percentage=prop.table(table(IMDB$binned_score2))*100)
barplot(table(train.data$binned_score2), xlab="Score bins", ylab="Occurences")
par(mfrow=c(1,1))

##numerical features##

#correlation among numerical features
correlations <- cor(train.data[,1:19])
corrplot(correlations, method = "circle", type="lower", addCoef.col="black", addCoefasPercent=TRUE, diag=FALSE, tl.srt=45, number.cex=.8)
rm(correlations)

#scatterplots of the numeric features plotted against IMDb scores
featurePlot(train.data[,c(14,8,3,5,13,4)], train.data$imdb_score, plot="scatter", layout=c(6,1), labels=c("Facebook like features",""))
featurePlot(train.data[,c(15:18)], train.data$imdb_score, plot="scatter", layout=c(4,1), labels=c("Appearance features",""))
featurePlot(train.data[,c(7,10,1)], train.data$imdb_score, plot="scatter", layout=c(3,1), labels=c("Voting and critics features",""))
featurePlot(train.data[,c(12,2,9,11,6)], train.data$imdb_score, plot="scatter", layout=c(5,1), labels=c("Movie specific features",""))

#boxplots of the numeric features for each score bin
featurePlot(train.data[,c(14,8,3,5,13,4)], train.data$binned_score2, plot="box", auto.key = list(columns = 4), layout=c(6,1), scales = list(y = list(relation="free"), x = list(rot = 90)), labels=c("Facebook like features",""))
featurePlot(train.data[,c(15:18)], train.data$binned_score2, plot="box", auto.key = list(columns = 4), layout=c(4,1), scales = list(y = list(relation="free"), x = list(rot = 90)), labels=c("Appearance features",""))
featurePlot(train.data[,c(7,10,1)], train.data$binned_score2, plot="box", auto.key = list(columns = 4), layout=c(3,1), scales = list(y = list(relation="free"), x = list(rot = 90)), labels=c("Voting and critics features",""))
featurePlot(train.data[,c(12,2,9,11,6)], train.data$binned_score2, plot="box", auto.key = list(columns = 4), layout=c(5,1), scales = list(y = list(relation="free"), x = list(rot = 90)), labels=c("Movie specific features",""))

##categorical features##

#barcharts of the categorical features subdivided by score bins
grid.arrange(ggplot(train.data, aes(country, fill=binned_score2)) + geom_bar(position="dodge"),
             ggplot(train.data, aes(color, fill=binned_score2)) + geom_bar(position="dodge"),
             ggplot(train.data, aes(content_rating, fill=binned_score2)) + geom_bar(position="dodge"),
             ggplot(train.data, aes(aspect_ratio, fill=binned_score2)) + geom_bar(position="dodge"),
             ncol=2, nrow=2)
tmp = lapply(factor_var[5:24], function(x) ggplot(train.data) + geom_bar(aes_string(x=x, fill="binned_score2"), position="dodge"))
grid.arrange(grobs=tmp, ncol=5, nrow=4)
rm(tmp)


##### Regression Analysis ######
lm.IMDB <- lm(imdb_score ~ ., data = IMDB)
summary(lm.IMDB)

#For Regression Analysis we can remove binned score variables as they aren't useful
IMDB_reg <- subset(IMDB, select = -c(binned_score1, binned_score2))

# Converting integer columns to numeric for ease of analysis
#IMDB_reg[, c(1,7,10)] <- lapply(IMDB_reg[, c(1,7,10)], as.numeric)
# Does color influence imdb_score?
table(IMDB_reg$color)
# Nearly 96% of all movies are colour which means that this variable is nearly constant as doesn't have
# a significant effect in determining imdb_score. Thus we can safely remove it.
IMDB_reg <- subset(IMDB_reg, select = -c(color))

#Simple linear regression example 
IMDB_eg <- lm(imdb_score ~ num_voted_users, data = IMDB_reg)
summary(IMDB_eg)
# ScatterPlot of IMDB Score VS Number of Voted Users
ggplot(IMDB_reg, aes(x=num_voted_users, y=imdb_score)) + geom_point() + 
  labs(x="Number of Voted Users", 
       y="IMDB Score") + geom_smooth(method='lm')

# Multiple Linear Regression of entire dataset
lm.IMDB_reg <- lm(imdb_score ~ ., data = IMDB_reg)
summary(lm.IMDB_reg)

# Computing the Variance Inflation Factors to detect Multicollinearity
library(car)
vif(lm.IMDB_reg)

# Correlation of "actor_1_facebook_likes", "cast_total_facebook_likes" and "actor_2_facebook_likes" 
# with IMDB Score
cor(IMDB_reg$imdb_score, IMDB_reg$actor_1_facebook_likes)
cor(IMDB_reg$imdb_score, IMDB_reg$actor_2_facebook_likes)
cor(IMDB_reg$imdb_score, IMDB_reg$cast_total_facebook_likes)
#Correlations are very similar

# Regressing IMDB Score by taking out each of the highly collinear variables to find out which model
# gives the best adjusted R- Squared
summary(lm(imdb_score ~. - cast_total_facebook_likes, data= IMDB_reg))
summary(lm(imdb_score ~. - actor_1_facebook_likes, data= IMDB_reg))
summary(lm(imdb_score ~. - actor_2_facebook_likes, data= IMDB_reg)) # gives best Adj. R-squared of 50.89%

# Removing actor_2_facebook_likes variable and actor_1_facebook_likes:
IMDB_reg = subset(IMDB_reg, select = -c(actor_1_facebook_likes,actor_2_facebook_likes))

#Adding Interactive Terms to relax Additive assumption
lm.IMDB_reg_add = lm(formula = imdb_score ~ . + num_voted_users:num_critic_for_reviews:num_user_for_reviews + 
                       movie_facebook_likes:cast_total_facebook_likes, data = IMDB_reg)
summary(lm.IMDB_reg_add)

#Rechecking VIF Scores
vif(lm.IMDB_reg_add)

# Diagnostic Plots for linear Regression to identify Issues and prescribe Improvements
library(ggfortify)
autoplot(lm.IMDB_reg_add)
par(mfrow=c(2,2))
plot(lm.IMDB_reg_add)

## Centering the dataset to prevent polynomial terms being highly correlated with X's
IMDBcenter<-IMDB_reg
IMDBcenter$num_critic_for_reviews=scale(IMDB$num_critic_for_reviews,scale = FALSE)
IMDBcenter$duration = scale(IMDB$duration,scale = FALSE)
IMDBcenter$director_facebook_likes = scale(IMDB$director_facebook_likes,scale = FALSE)
IMDBcenter$actor_3_facebook_likes = scale(IMDB$actor_3_facebook_likes,scale = FALSE)
IMDBcenter$gross = scale(IMDB$gross,scale = FALSE)
IMDBcenter$num_voted_users = scale(IMDB$num_voted_users,scale = FALSE)
IMDBcenter$cast_total_facebook_likes = scale(IMDB$cast_total_facebook_likes,scale = FALSE)
IMDBcenter$facenumber_in_poster = scale(IMDB$facenumber_in_poster,scale = FALSE)
IMDBcenter$num_user_for_reviews = scale(IMDB$num_user_for_reviews,scale = FALSE)
IMDBcenter$budget = scale(IMDB$budget,scale = FALSE)
IMDBcenter$title_year = scale(IMDB$title_year,scale = FALSE)
IMDBcenter$movie_facebook_likes = scale(IMDB$movie_facebook_likes,scale = FALSE)

# Adding Polynomial terms to remedy the Non-Linearity Issue
lm.IMDB_reg_add_lin =lm(formula = imdb_score ~ . + num_voted_users:num_critic_for_reviews:num_user_for_reviews + 
                          movie_facebook_likes:cast_total_facebook_likes + I(duration^2) + I(num_voted_users^2) + 
                          I(num_critic_for_reviews^2) + I(num_user_for_reviews^2) + I(title_year^2) + 
                          I(movie_facebook_likes^2)+ I(gross^2) + I(duration^3) + I(num_voted_users^3) + 
                          I(num_critic_for_reviews^3) + I(num_user_for_reviews^3) + I(title_year^3) + 
                          I(movie_facebook_likes^3)+ I(gross^3) , data = IMDBcenter)

summary(lm.IMDB_reg_add_lin)

#Addressing the Non-Normality and Heteroscedasticity Issue using the Box-Cox transform
library(MASS)
b = boxcox(lm.IMDB_reg_add_lin, lambda = seq(-5,5,0.5), ylab = "Log-Likelihood", xlab = "lambda")
lambda_b = b$x
log_like = b$y
# Which value of lambda gives the highest possible log-likelihood?
max_lambda= b$x[which.max(b$y)]

# Running the linear model after transforming the response using Box-Cox
lm.IMDB_reg_add_lin_norm =lm(formula = (imdb_score^3 -1)/3 ~ . + num_voted_users:num_critic_for_reviews:num_user_for_reviews + 
                               movie_facebook_likes:cast_total_facebook_likes + I(duration^2) + I(num_voted_users^2) + 
                               I(num_critic_for_reviews^2) + I(num_user_for_reviews^2) + I(title_year^2) + 
                               I(movie_facebook_likes^2)+ I(gross^2) + I(duration^3) + I(num_voted_users^3) + 
                               I(num_critic_for_reviews^3) + I(num_user_for_reviews^3) + I(title_year^3) + 
                               I(movie_facebook_likes^3)+ I(gross^3) , data = IMDBcenter)

summary(lm.IMDB_reg_add_lin_norm)

#Before and after plots showing that Non-Linearity has been addressed
par(mfrow=c(1,2))
plot(lm.IMDB_reg_add, which = 1)
plot(lm.IMDB_reg_add_lin, which = 1)

#Before and after plots showing that Normality has been addressed
par(mfrow=c(1,2))
plot(lm.IMDB_reg_add_lin, which = 2)
plot(lm.IMDB_reg_add_lin_norm, which = 2)

#Before and after plots showing that Heteroscedasticity has been removed
par(mfrow=c(1,2))
plot(lm.IMDB_reg_add_lin, which = 3)
plot(lm.IMDB_reg_add_lin_norm, which = 3)

# Double Check that data is Normal using Normal Probability Plot
qqnorm(rstandard(lm.IMDB_reg_add_lin_norm))
qqline(rstandard(lm.IMDB_reg_add_lin_norm))

# Making predictions from the Linear Model
#Training and test split with ratio 80:20
set.seed(1)
split <- sample(seq_len(nrow(IMDB_reg)), size = floor(0.80*nrow(IMDB_reg)))
#We transform the response so that it is inline with the Box-Cox formula
IMDB_reg_trans <- IMDB_reg
IMDB_reg_trans$imdb_score <- (IMDB_reg_trans$imdb_score^3-1)/3
train_set<- IMDB_reg_trans[split,]
test_set <- IMDB_reg_trans[-split,]
dim(train_set)
dim(test_set)

#Run model on Training data and use it to predict test data
lm.trainmod <- lm(formula = (imdb_score) ~ . + num_voted_users:num_critic_for_reviews:num_user_for_reviews + 
                    movie_facebook_likes:cast_total_facebook_likes + I(duration^2) + I(num_voted_users^2) + 
                    I(num_critic_for_reviews^2) + I(num_user_for_reviews^2) + I(title_year^2) + 
                    I(movie_facebook_likes^2)+ I(gross^2) + I(duration^3) + I(num_voted_users^3) + 
                    I(num_critic_for_reviews^3) + I(num_user_for_reviews^3) + I(title_year^3) + 
                    I(movie_facebook_likes^3)+ I(gross^3), data = train_set)
imdb_score_pred <- predict(lm.trainmod, test_set[,-17])

#Calculate prediction accuracy
act_pred <- data.frame(cbind(actual = test_set$imdb_score, predicted = imdb_score_pred))
# Note that since the data has been trasnformed, IMDB Score will no longer be in the range of 0-10.
head(act_pred)
cor(act_pred)
ggplot(data = act_pred, aes(x= actual, y = predicted)) + geom_point() + geom_smooth(method = "lm") 

#Min max accuracy
min_max_accuracy <- mean(apply(act_pred,1,min)/apply(act_pred,1,max))
min_max_accuracy

#MeanAbsoultePercentageError
mape = mean(abs(act_pred$actual-act_pred$predicted)/act_pred$actual)
mape
####classification####

##preparation

#3 times repeated 10 fold cross validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)

#common seed for all methods
seed = 1

#common metric on which performance is evaluated
metric = "Kappa"

#datasets used for tuning, training and testing different models
class.train.data = subset(train.data, select=-c(imdb_score, binned_score1))
class.train.trans.data = subset(train.data.trans, select=-c(imdb_score, binned_score1))
class.test.data = subset(test.data, select=-c(imdb_score, binned_score1))
class.test.trans.data = subset(test.data.trans, select=-c(imdb_score, binned_score1))

##Logistic Regression

#Multinomial Logistic Regression (One vs All)
#training
start_time<-Sys.time()
set.seed(seed)
log.model = train(binned_score2~., data=class.train.data, trControl=control, method="multinom", metric=metric)
end_time<-Sys.time()
log.time.train= end_time - start_time

#prediction
start_time<-Sys.time()
log.pred = predict(log.model, class.test.data, "raw")
end_time<-Sys.time()
log.time.test= end_time - start_time

#performance evaluation
log.confusion=confusionMatrix(table(log.pred, class.test.data$binned_score2))
log.confusion

#alternative approach
# set.seed(seed)
# log.model = multinom(binned_score2~., data=class.train.data, metric=metric)
# summary(log.model)
# log.pred = predict(log.model, class.test.data, "class")
# confusionMatrix(table(log.pred, class.test.data$binned_score2))

##SVM

# SVM - Linear

#tuning
grid <- expand.grid(C = c(.01, .1, 0.5, 0.75, 1, 2, 5))
start_time<-Sys.time()
set.seed(seed)
svm1.tune<-train(binned_score2~., data=class.train.trans.data, method="svmLinear", metric=metric, tuneGrid=grid)
end_time<-Sys.time()
svm1.time.tune=end_time - start_time
plot(svm1.tune)

#training
start_time<-Sys.time()
set.seed(seed)
svm1.model<-train(binned_score2~., data=class.train.trans.data, method="svmLinear", metric=metric, tuneGrid=expand.grid(C = svm1.tune$bestTune[[1]]), trControl=control)
end_time<-Sys.time()
svm1.time.train=end_time - start_time

#predicting
start_time<-Sys.time()
svm1.pred=predict(svm1.model,class.test.trans.data)
end_time<-Sys.time()
svm1.time.test=end_time - start_time

#performance evaluation
svm1.confusion=confusionMatrix(table(svm1.pred,class.test.data$binned_score2))
svm1.confusion

# SVM - Polynomial

#tuning
grid <- expand.grid(C = c(100, 200, 300), degree=c(1,2,3), scale = c(.0001, .001, .01, .1))
start_time<-Sys.time()
set.seed(seed)
svm2.tune<-train(binned_score2~., data=class.train.trans.data, method="svmPoly", metric=metric, tuneGrid=grid)
end_time<-Sys.time()
svm2.time.tune=end_time- start_time
plot(svm2.tune)

#training
start_time<-Sys.time()
set.seed(seed)
svm2.model<-train(binned_score2~., data=class.train.trans.data,  method="svmPoly", metric=metric, tuneGrid=expand.grid(degree=svm2.tune$bestTune[[1]],scale = svm2.tune$bestTune[[2]],C = svm2.tune$bestTune[[3]]), trControl=control)
end_time<-Sys.time()
svm2.time.train=end_time - start_time

#predicting
start_time<-Sys.time()
svm2.pred=predict(svm2.model,class.test.trans.data)
end_time<-Sys.time()
svm2.time.test=end_time - start_time

#performance evaluation
svm2.confusion=confusionMatrix(table(svm2.pred,class.test.trans.data$binned_score2))
svm2.confusion

# SVM - Radial Kernel

#tuning
grid <- expand.grid(sigma = c(.0001, .01, 1, 10), C = c(.01, .1, 1, 10, 100))
start_time<-Sys.time()
set.seed(seed)
svm3.tune <- train(binned_score2~., class.train.trans.data, method = "svmRadial", metric=metric, tuneGrid = grid)
end_time<-Sys.time()
svm3.time.tune=end_time - start_time
plot(svm3.tune)

#training
start_time<-Sys.time()
set.seed(seed)
svm3.model<-train(binned_score2~., data=class.train.trans.data, method="svmRadial", metric=metric, tuneGrid=expand.grid(sigma=svm3.tune$bestTune[[1]], C = svm3.tune$bestTune[[2]]), trControl=control)
end_time<-Sys.time()
svm3.time.train=end_time - start_time

#predicting
start_time<-Sys.time()
svm3.pred=predict(svm3.model,class.test.trans.data)
end_time<-Sys.time()
svm3.time.test=end_time - start_time

#performance evaluation
svm3.confusion=confusionMatrix(table(svm3.pred,class.test.trans.data$binned_score2))
svm3.confusion

##LDA

#training
start_time<-Sys.time()
set.seed(seed) 
lda.model = train(binned_score2~., data=class.train.trans.data, trControl=control, method="lda", metric=metric)
end_time<-Sys.time()
lda.time.train= end_time - start_time

#prediction
start_time<-Sys.time()
lda.pred = predict(lda.model, class.test.trans.data, "raw")
end_time<-Sys.time()
lda.time.test= end_time - start_time

#performance evaluation
lda.confusion=confusionMatrix(table(lda.pred, class.test.trans.data$binned_score2))
lda.confusion

#alternative approach
# lda.model <- lda(binned_score2~., class.train.data)
# lda.pred <- predict(lda.model, class.test.data)
# confusionMatrix(table(lda.pred$class, class.test.data$binned_score2))

##k-Nearest Neighbours

#Principal Component Analysis
x<-model.matrix(binned_score2~.,subset(IMDBtrans,select=-c(binned_score1,imdb_score)))
x<-as.data.frame(x)
x=x[,-1]

pca.comp=prcomp(x[train,])
pca.comp$rotation

pca.var=(pca.comp$sdev)^2
pca.prop.var=pca.var/sum(pca.var)
pca.prop.var[pca.prop.var>=0.005]
sum(pca.prop.var[pca.prop.var>=0.005])
plot(pca.prop.var,type='b')
plot(cumsum(pca.prop.var),type='b')

#using the top principal components that explain 95% variance in the data to tune and train knn model
knn.data.train=data.frame(binned_score2=class.train.data$binned_score2,pca.comp$x)
knn.data.test=predict(pca.comp,x[-train,])
knn.data.test=as.data.frame(knn.data.test)
knn.data.test=knn.data.test[,1:26]

#tuning
start_time=Sys.time()
set.seed(seed)
knn.tune = train(binned_score2~., knn.data.train[,1:27], method = "knn", metric=metric, tuneLength=10)
end_time=Sys.time()
knn.time.tune=end_time - start_time
plot(knn.tune)

#training
start_time<-Sys.time()
set.seed(seed)
knn.model= train(binned_score2~., knn.data.train[,1:27], method = "knn", tuneGrid=expand.grid(k = knn.tune$bestTune[[1]]), trControl=control)
end_time=Sys.time()
knn.time.train=end_time - start_time

#testing
start_time=Sys.time()
knn.pred = predict(knn.model,knn.data.test)
end_time=Sys.time()
knn.time.test=end_time - start_time

#performance evaluation
knn.confusion = confusionMatrix(table(knn.pred, class.test.trans.data$binned_score2))
knn.confusion

##Random Forests

#preparation
mtry <- floor(sqrt(ncol(class.train.data)))
mtry
tunegrid <- expand.grid(.mtry=c(mtry,seq(mtry+1,41,2)))

#tuning
start_time<-Sys.time()
set.seed(seed)
rf.tune <- train(binned_score2~., data=class.train.data, method="rf", metric=metric,  ntree=100)
end_time<-Sys.time()
rf.time.tune = end_time - start_time
print(rf.tune)
plot(rf.tune)
summary(rf.tune)
rf.tune$results

#training
start_time<-Sys.time()
set.seed(seed)
rf.model = randomForest(binned_score2~., data=class.train.data, mtry=rf.tune$bestTune[[1]], ntree=5000)
end_time<-Sys.time()
rf.time.train = end_time - start_time
rf.model
varImpPlot(rf.model)
plot(rf.model, main="")
legend("topright", colnames(rf.model$err.rate), col = 1:5, fill = 1:5, cex=0.8)

#predicting
start_time<-Sys.time()
rf.pred=predict(rf.model,class.test.data)
end_time<-Sys.time()
rf.time.test = end_time - start_time

#performance evaluation
rf.confusion = confusionMatrix(table(rf.pred,class.test.data$binned_score2))
rf.confusion

##Boosting

#tune
tunegrid<-expand.grid(interaction.depth=c(3:7),n.trees=c(100,150,200,300),shrinkage=c(0.01,0.05,0.1),n.minobsinnode=c(3,5,7))
start_time=Sys.time()
set.seed(seed)
boost.tune<-train(binned_score2~., class.train.trans.data, method='gbm', tuneGrid=tunegrid, metric=metric)
end_time=Sys.time()
boost.time.tune=end_time- start_time

#training
tunegrid<-expand.grid(interaction.depth=boost.tune$bestTune[[2]],n.trees=boost.tune$bestTune[[1]],shrinkage=boost.tune$bestTune[[3]],n.minobsinnode=boost.tune$bestTune[[4]])
start_time=Sys.time()
set.seed(seed)
boost.model<-train(binned_score2~., class.train.trans.data, method='gbm', trControl=control, tuneGrid=tunegrid, metric=metric)
end_time=Sys.time()
boost.time.train=end_time- start_time

#predicting
start_time<-Sys.time()
boost.pred <- predict(boost.model,class.test.trans.data)
end_time<-Sys.time()
boost.time.test = end_time - start_time

#performance evaluation
boost.confusion = confusionMatrix(table(boost.pred,class.test.trans.data$binned_score2))
boost.confusion

##Performance summary

#Multinomial Logistic Regression
log.time.train
log.time.test
log.confusion

#LDA
lda.time.train
lda.time.test
lda.confusion

#k-Nearest Neighbours
knn.time.tune
knn.time.train
knn.time.test
knn.confusion

#SVM - Linear
svm1.time.tune
svm1.time.train
svm1.time.test
svm1.confusion

#SVM - Polynomial
svm2.time.tune
svm2.time.train
svm2.time.test
svm2.confusion

#SVM - Radial Kernel
svm3.time.tune
svm3.time.train
svm3.time.test
svm3.confusion

#Random Forests
rf.time.tune
rf.time.train
rf.time.test
rf.confusion

#Boosting
boost.time.tune
boost.time.train
boost.time.test
boost.confusion