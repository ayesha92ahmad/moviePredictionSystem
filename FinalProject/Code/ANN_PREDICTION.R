library(neuralnet)
movie_data = read.csv("movie_metadata.csv")
str(movie_data)
movie_data$title_year = as.factor(movie_data$title_year)
num_dat = sapply(movie_data, is.numeric)  
movie_num = movie_data[, num_dat] 
hist(movie_num$imdb_score,breaks = 20) 

movie_num_new <- na.omit(movie_num) 
scaled_data <- data.frame(lapply(movie_num_new, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE))))
scaled_data$imdb_score = movie_num_new$imdb_score / 10
scaled_data <- scaled_data[,c("imdb_score","director_facebook_likes","duration","actor_1_facebook_likes","actor_2_facebook_likes","actor_3_facebook_likes","facenumber_in_poster","budget")]

scaled_data = data.frame(scaled_data)
index <- 1:nrow(scaled_data)
index_test <- sample(index, trunc(length(index)*0.25))
index_test1 <- sample(index, trunc(length(index)*0.2))
index_test2 <- sample(index, trunc(length(index)*0.4))
index_test3 <- sample(index, trunc(length(index)*0.5))

test_data <- scaled_data[index_test,]
train_data <- scaled_data[-index_test,]
f <- as.formula(paste0('imdb_score ~ ',paste(names(train_data[!names(train_data) %in% 'imdb_score']), collapse = ' + ')))
ann_model <- neuralnet(f, train_data, hidden = 5)

test_data1 <- scaled_data[index_test1,]
train_data1 <- scaled_data[-index_test1,]
f1 <- as.formula(paste0('imdb_score ~ ',paste(names(train_data1[!names(train_data1) %in% 'imdb_score']), collapse = ' + ')))
ann_model1 <- neuralnet(f1, train_data1, hidden = 5)

test_data2 <- scaled_data[index_test2,]
train_data2 <- scaled_data[-index_test2,]
f2 <- as.formula(paste0('imdb_score ~ ',paste(names(train_data2[!names(train_data2) %in% 'imdb_score']), collapse = ' + ')))
ann_model2 <- neuralnet(f2, train_data2, hidden = 5)

test_data3 <- scaled_data[index_test3,]
train_data3 <- scaled_data[-index_test3,]
f3 <- as.formula(paste0('imdb_score ~ ',paste(names(train_data3[!names(train_data3) %in% 'imdb_score']), collapse = ' + ')))
ann_model3 <- neuralnet(f3, train_data3, hidden = 5)


par(mfrow = c(2,2)) 
plot(ann_model)
plot(ann_model1)
plot(ann_model2)
plot(ann_model3)

pred <- compute(ann_model, test_data[,-1])
mse <- mean((test_data$imdb_score - pred$net.result)^2)

pred1 <- compute(ann_model1, test_data1[,-1])
mse1 <- mean((test_data1$imdb_score - pred1$net.result)^2)

pred2 <- compute(ann_model2, test_data2[,-1])
mse2 <- mean((test_data2$imdb_score - pred2$net.result)^2)

pred3 <- compute(ann_model3, test_data3[,-1])
mse3 <- mean((test_data3$imdb_score - pred3$net.result)^2)

print(mse)
print(mse1)
print(mse2)
print(mse3)

# Plot the predicted values v/s actual values
par(mfrow = c(2,2))
plot(pred$net.result,test_data$imdb_score,col=c("red","blue"), xlab="Predicted",ylab="Actual", main="Train 75% Test 25%")
plot(pred1$net.result,test_data1$imdb_score,col=c("red","blue"), xlab="Predicted",ylab="Actual", main="Train 80% Test 20%")
plot(pred2$net.result,test_data2$imdb_score,col=c("red","blue"), xlab="Predicted",ylab="Actual", main="Train 60% Test 40%")
plot(pred3$net.result,test_data3$imdb_score,col=c("red","blue"), xlab="Predicted",ylab="Actual", main="Train 50% Test 50%")
