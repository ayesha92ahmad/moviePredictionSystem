library(randomForest)
library(pROC)

library(mlbench)
library(caret)
movies <- read_csv("~/Downloads/Datamining/movie_rating_prediction/movie_metadata.csv")
set.seed(0)
movies_with_good_variables = movies[, c("imdb_score",
                                        "director_facebook_likes", 
                                        "cast_total_facebook_likes", 
                                        "actor_1_facebook_likes",
                                        "actor_2_facebook_likes",
                                        "actor_3_facebook_likes",
                                        "movie_facebook_likes", 
                                        "facenumber_in_poster",
                                        "gross",
                                        "budget")]
mvs = na.omit(movies_with_good_variables)
#finding mean imdb score
mean_rating <- mean(mvs$imdb_score)
#discretization of imdb score using mean
#if the score is less than "floor of the mean -1" then rating is poor else if score  greater 
#than ceiling of mean rating is good. else rating is average
mvs$rating <- ifelse(mvs$imdb_score < (floor(mean_rating)-1),'poor',ifelse(mvs$imdb_score > ceiling(mean_rating),'good', 'average'))
#convert to factors 
mvs$rating <- as.factor(mvs$rating)
#66.32% of data is recommended to be ideal training size for random forest hence 36.8% for test

index = sample(1:nrow(mvs), size=0.368*nrow( mvs))
test = mvs[index,]
train = mvs[-index,]
#performing random forest on training set. Not including imdb score now
movie.rf <- randomForest(rating ~. -imdb_score, train, replace=TRUE,na.action=na.omit , mtry = 5, ntree= 100)

#first check on training set if prediction is happening correctly
movie_train_prediction <- predict(movie.rf, train)
table(train$rating, movie_train_prediction)
#above table will show if prediction model is correct on training data
#perform prediction on test data
movie_test_prediction <- predict(movie.rf, test)
#show the confusion matrix of test data
table(test$rating, movie_test_prediction)
#show weightage of each attribute in classifier model to know the significance of variable
#show accuracy
control <- trainControl(method="repeatedcv", number=2, repeats=2, search="grid")
set.seed(7)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
#mtry <- sqrt(ncol(train))
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes
tunegrid <- expand.grid(.mtry=c(2:4), .ntree=c(100, 200))
rf_gridsearch <- train(rating ~. -imdb_score, data=train, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
pred<- predict(rf_gridsearch, train)
table(pred, train$rating)
pred<- predict(rf_gridsearch, test)
table(pred, test$rating)
rightPred <- pred == test$rating
accuracy <- sum(rightPred)/nrow(test)
plot(rf_gridsearch)
multiclass.roc(as.numeric(test$rating), as.numeric(pred))
