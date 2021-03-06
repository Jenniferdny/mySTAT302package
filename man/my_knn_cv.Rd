% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_cnn_cv.R
\name{my_knn_cv}
\alias{my_knn_cv}
\title{K-Nearest Neighbors Cross Validation}
\usage{
my_knn_cv(train, trueValues, n, f)
}
\arguments{
\item{train}{DataFrame, input including the training variable.}

\item{trueValues}{Vector, input the true class value of training data.}

\item{n, }{Numeric input, is the number of neighbors.}

\item{f}{Numeric input folds.}
}
\value{
a list includes \code{class}, a vector of the predicted class y-hat
for all observations, \code{cv_err}, a numeric of the cross-validation
misclassification error.
}
\description{
Function doing Cross Validation test for the KNN model.
}
\examples{
penguins_data <- na.omit(my_penguins)
train <- penguins_data[, 3:6]
trueValues <- penguins_data$species
my_knn_cv(train, trueValues, 5, 5)

}
\keyword{prediction}
