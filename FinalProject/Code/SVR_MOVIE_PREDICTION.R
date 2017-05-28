library("e1071")
# Read the IMDB dataset
movies <- read.csv("movie_metadata.csv")

# Get info about types of attributes from the dataset
str(movies)

# Convert title_year from numeric to factor type since we don't consider this attribute for prediction
movies$title_year = as.factor(movies$title_year)
str(movies)

#Extract only the numeric data columns
number_attributes <- sapply(movies,is.numeric)
number_attributes
movies_num <- movies[,number_attributes]     # movies_num contains only numeric data
hist(movies$imdb_score, breaks=30)
plot(density(movies$imdb_score))
abline(v=mean(movies$imdb_score), lty=2)  

# Now we know that most scores lie between somewhere close to 6 and 7.5
movies_1 <- na.omit(movies_num)    #Remove the rows with missing values
scaled_data <- data.frame(lapply(movies_1, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE))))                         #Scale the data
scaled_data$imdb_score = movies_1$imdb_score / 10
scaled_data <- scaled_data[,c("imdb_score","director_facebook_likes","duration","actor_1_facebook_likes","actor_2_facebook_likes","actor_3_facebook_likes","facenumber_in_poster","budget")]

index<- 1:nrow(scaled_data)
index_test <- sample(index, trunc(length(index)*0.25))
index_test1 <- sample(index, trunc(length(index)*0.2))
index_test2 <- sample(index, trunc(length(index)*0.4))
index_test3 <- sample(index, trunc(length(index)*0.5))
# Split the data to train and test set
test_data <- scaled_data[index_test,]
train_data <- scaled_data[-index_test,]

# Create a model using SVM
#svm_pred <-svm(imdb_score ~., data = train_data, kernel = "radial")
#svm_pred <-svm(imdb_score ~., data = train_data, kernel = "linear")
#svm_pred <-svm(imdb_score ~., data = train_data, kernel = "sigmoid")
#svm_pred <-svm(imdb_score ~., data = train_data, kernel = "polynomial")
#svm_pred <-svm(imdb_score ~., data = train_data, kernel = "radial", gamma = 0.5)
svm_pred <-svm(imdb_score ~., data = train_data, kernel = "radial", gamma = 0.7)
# Apply the model on test data and get the predicted values
svm_predictions <- predict(svm_pred, test_data[,-1])
mean((test_data$imdb_score - svm_predictions)^2)     # Get the mean squared error

test_data1 <- scaled_data[index_test1,]
train_data1 <- scaled_data[-index_test1,]
svm_pred1 <-svm(imdb_score ~., data = train_data1, kernel = "radial", gamma = 0.7)
svm_predictions1 <- predict(svm_pred1, test_data1[,-1])
mean((test_data1$imdb_score - svm_predictions1)^2)

test_data2 <- scaled_data[index_test2,]
train_data2 <- scaled_data[-index_test2,]
svm_pred2 <-svm(imdb_score ~., data = train_data2, kernel = "radial", gamma = 0.7)
svm_predictions2 <- predict(svm_pred2, test_data2[,-1])
mean((test_data2$imdb_score - svm_predictions2)^2)

test_data3 <- scaled_data[index_test3,]
train_data3 <- scaled_data[-index_test3,]
svm_pred3 <-svm(imdb_score ~., data = train_data3, kernel = "radial", gamma = 0.7)
svm_predictions3 <- predict(svm_pred3, test_data3[,-1])
mean((test_data3$imdb_score - svm_predictions3)^2)

# Plot the predicted values v/s actual values
par(mfrow = c(2,2))
plot(svm_predictions,test_data$imdb_score,col=c("red","blue"), xlab="Predicted",ylab="Actual", main="Train 75% Test 25%")
plot(svm_predictions1,test_data1$imdb_score,col=c("red","blue"), xlab="Predicted",ylab="Actual", main="Train 80% Test 20%")
plot(svm_predictions2,test_data2$imdb_score,col=c("red","blue"), xlab="Predicted",ylab="Actual", main="Train 60% Test 40%")
plot(svm_predictions3,test_data3$imdb_score,col=c("red","blue"), xlab="Predicted",ylab="Actual", main="Train 50% Test 50%")
