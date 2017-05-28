getwd()
setwd("D:/PhD/Spring 2017/DM/project/movie_metadata.csv")

#reading csv file
movies <- read.csv("movie_metadata.csv")
#information about type of attributes in movie
str(movies)
# get the set of numeric attributes
numeric_attributes<-sapply(movies,is.numeric)

#attributes containing only numeric data
movies_numeric <- movies[,numeric_attributes]

#removing missing values
movies_missing_removed <- na.omit(movies_numeric)

#movie_data scaled
scaled_movie_data <- data.frame(lapply(movies_missing_removed, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE))))  

index_start<- 1:nrow(scaled_movie_data)

#For 50-50
#splitting into training and test data 
index_test50 <- sample(index_start, trunc(length(index_start)*0.50))
test_data50 <- scaled_movie_data[index_test50,]

index_train50 <- sample(index_start, trunc(length(index_start)*0.50))

train_data50 <- scaled_movie_data[index_train50,]


#create model using linear regression

linear_model <-glm(imdb_score ~., data = train_data50)

#applying the model created on test data to obtain predictions
linear_predictions1 <- predict(linear_model, test_data50)

#plot(linear_predictions,test_data50$imdb_score,col=c("red","green"), xlab="Predicted",ylab="Actual")

#root mean square error
mean((test_data50$imdb_score - linear_predictions)^2) 


#For 60-40
#splitting into training and test data 
index_test40 <- sample(index_start, trunc(length(index_start)*0.40))
test_data40 <- scaled_movie_data[index_test40,]

index_train60 <- sample(index_start, trunc(length(index_start)*0.60))

train_data60 <- scaled_movie_data[index_train60,]


#create model using linear regression

linear_model <-glm(imdb_score ~., data = train_data60)

#applying the model created on test data to obtain predictions
linear_predictions2 <- predict(linear_model, test_data40)

#plot(linear_predictions,test_data40$imdb_score,col=c("red","green"), xlab="Predicted",ylab="Actual")

#root mean square error
mean((test_data40$imdb_score - linear_predictions)^2) 

#For 75-25
#splitting into training and test data 
index_test25 <- sample(index_start, trunc(length(index_start)*0.25))
test_data25 <- scaled_movie_data[index_test25,]

index_train75 <- sample(index_start, trunc(length(index_start)*0.75))

train_data75 <- scaled_movie_data[index_train75,]


#create model using linear regression

linear_model <-glm(imdb_score ~., data = train_data75)

#applying the model created on test data to obtain predictions
linear_predictions3 <- predict(linear_model, test_data25)

#plot(linear_predictions,test_data25$imdb_score,col=c("red","green"), xlab="Predicted",ylab="Actual")

#root mean square error
mean((test_data25$imdb_score - linear_predictions)^2) 


#For 80-20
#splitting into training and test data 
index_test20 <- sample(index_start, trunc(length(index_start)*0.20))
test_data20 <- scaled_movie_data[index_test20,]

index_train80 <- sample(index_start, trunc(length(index_start)*0.80))

train_data80 <- scaled_movie_data[index_train80,]


#create model using linear regression

linear_model <-glm(imdb_score ~., data = train_data80)

#applying the model created on test data to obtain predictions
linear_predictions4 <- predict(linear_model, test_data20)

#plot(linear_predictions,test_data20$imdb_score,col=c("red","green"), xlab="Predicted",ylab="Actual")

#root mean square error
mean((test_data20$imdb_score - linear_predictions)^2) 

par(mfrow=c(2,2))
plot(linear_predictions1,test_data50$imdb_score,col=rainbow(2), xlab="Predicted",ylab="Actual", main="Test data 50")
plot(linear_predictions2,test_data40$imdb_score,col=rainbow(2), xlab="Predicted",ylab="Actual", main= "Test data 40")
plot(linear_predictions3,test_data25$imdb_score,col=rainbow(2), xlab="Predicted",ylab="Actual", main="Test data 25")
plot(linear_predictions4,test_data20$imdb_score,col=rainbow(2), xlab="Predicted",ylab="Actual",main="Test data 20")
