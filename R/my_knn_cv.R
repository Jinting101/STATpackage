#' Prediction Function
#'
#' This function uitilizes cross validation and nearest validation to predict the output
#'
#' @param train training data set
#' @param cl true class value of the training data
#' @param k_nn number of nearest neighbors
#' @param k_cv number of folds used in cross validation
#'
#' @import class
#'
#' @return A vector of predicted class and a numeric with cross validation classification error
#'
#' @examples
#' train = mtcars[, 2:6]
#' cl = mtcars[, 1]
#' my_knn_cv(train, cl, 5, 5)
#'
#' @export

# function: my_knn_cv, use cross validation and k-nearest neignbors to perform prediction
# input: a training data set, true class value of training data, integer representing
#         number of neighbors and integer representing number of folds for CV
# output: a vector of predicted class and a numeric with cross validation classification error
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # randomly assign observations to k groups
  fold = sample(rep(1:k_cv, length = nrow(train)))
  cl = as.data.frame(cl)
  mis_rate = 0;
  for (i in 1:k_cv) {
    # train a model using training sets and test sets split by fold
    index = which(fold == i)
    data_train = train[-index, ]
    data_test = train[index, ]
    train_cl = cl[-index, ]
    test_cl= cl[index, ]
    # calculate the predication
    pred = knn(data_train, data_test, train_cl, k_nn)
    # calculate the misclassified rate
    mis_rate[i] = mean(pred != test_cl)
  }
  # predict using full data as training and test data
  class = knn(train, train, cl$cl, k_nn)
  # calculate the average misclassified rate
  avg_miss = mean(mis_rate)
  return(list("class" = as.vector(class), "cv_err" = avg_miss))
}
