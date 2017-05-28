#Visualizations
library(readr)
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyr)
library(rworldmap)
library(classInt)
library(RColorBrewer)
movies <- read_csv("~/Downloads/movie_metadata.csv")
ggplot(data=movies,aes(x = title_year))+geom_histogram(binwidth=1)
ggplot(data=movies,aes(x = duration))+geom_histogram(binwidth=3)+xlim(c(0,quantile(movies$duration,0.99, na.rm = T)))
movies <-mutate(movies, lenOfTitle=nchar(as.character(movie_title)))
ggplot(data=movies,aes(x = lenOfTitle))+ geom_histogram(binwidth=3)+ xlim(c(0,quantile(movies$lenOfTitle,1, na.rm = T)))
#correlation analysis:
num = sapply(movies, is.numeric)
fact = sapply(movies, is.factor)
imdb_numeric = movies[, num]
imdb_factor = movies[, fact]
M<- cor(na.omit(imdb_numeric), use="complete.obs", method="pearson")
corrplot(M, method="circle")
movie<-movies
genres <-movie$genres
genre_df <- movie %>% separate_rows(genres, sep = "\\|") %>% group_by(trimws(genres, which= c("both"))) %>% summarise(Ratings = mean(imdb_score)) %>% arrange(desc(Ratings))
genre_number <-  movie %>% separate_rows(genres, sep = "\\|") %>% group_by(trimws(genres, which= c("both"))) %>% summarise(Number_of_movies = n()) %>% arrange(desc(Number_of_movies))
country_rating <- movie %>% separate_rows(country, sep = "\\|") %>% group_by(country=trimws(country, which= c("both"))) %>% summarise(Ratings = mean(imdb_score)) %>% arrange(desc(Ratings))
country_number <- movie %>% separate_rows(country, sep = "\\|") %>% group_by(country=trimws(country, which= c("both"))) %>% summarise(Number_of_movies = n()) %>% arrange(desc(Number_of_movies))
country_number_data<- head(country_number, 50)
country_rating_data <-head(country_rating, 50)
#country_data contains name and number of movies for top 15 countries with max number of movies 
sPDF <- joinCountryData2Map( country_number_data
                             ,joinCode='NAME'
                             ,nameJoinColumn = "country"
                             ,verbose = TRUE )
ratingMap<- joinCountryData2Map(country_rating_data, joinCode = 'NAME'
                                , nameJoinColumn = "country"
                                ,verbose=TRUE)
mapDevice() #create world map shaped window
#14 is minimum number and 3807 is max
mapCountryData(sPDF,nameColumnToPlot = "Number_of_movies")
mapDevice()
mapCountryData(ratingMap, nameColumnToPlot = "Ratings")


