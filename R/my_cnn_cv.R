#' K-Nearest Neighbors Cross Validation
#'
#' Function doing Cross Validation test for the KNN model.
#'
#' @param train DataFrame, input including the training variable.
#' @param trueValues Vector, input the true class value of training data.
#' @param n, Numeric input, is the number of neighbors.
#' @param f Numeric input folds.
#'
#' @keywords prediction
#'
#' @return a list includes \code{class}, a vector of the predicted class y-hat
#'   for all observations, \code{cv_err}, a numeric of the cross-validation
#'   misclassification error.
#'
#' @examples
#' penguins_data <- na.omit(my_penguins)
#' train <- penguins_data[, 3:6]
#' trueValues <- penguins_data$species
#' my_knn_cv(train, trueValues, 5, 5)
#'
#' @export
my_knn_cv <- function(train, trueValues, n, f) {
  splitData <- sample(rep(1:f, length = nrow(train))) # random split data
  data <- data.frame("x" = train, "y" = trueValues, "split" = splitData)

  # f folds Cross Validation test
  n = ncol(data) - 2
  predict_list <- list()
  rate_vec <- vector()
  for (i in 1:fold) {
    # Construct the training data
    data_train <- data %>% dplyr::filter(split != i)
    data_train <- data_train[, 1:n]
    data_test <- data %>% dplyr::filter(split == i)
    data_test <- data_test[, 1:n]
    f1 <- factor(as.vector(trueValues[splitData != i]))

    # Train our models
    predict_list[[i]] <- knn(data_train, data_test, f1, k = n)
    # misclassification rate
    rate_vec[i] <- sum(predict_list[[i]] != trueValues[splitData == i]) / length(trueValues[splitData == i])
  }

  # Use full data to train the model
  f1 <- factor(as.vector(trueValues))
  class <- knn(data[, 1:n], data[, 1:n], trueValues, n)

  cv_error <- mean(rate_vec)

  return(list(class, cv_error))
}
