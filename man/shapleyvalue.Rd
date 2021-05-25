\name{shapleyvalue}
\alias{shapleyvalue}
\title{ShapleyValueRegression -- to calculate the relative importance of attributes in linear regression}
\description{
Shapley Value Regression for calculating the relative importance of independent variables in linear regression with avoiding the collinearity.
}
\arguments{
y                A coloumn or data set of the dependent variable

x                A matrix or data set of the independent variables
}
\examples{
library(MASS)
library(tidyverse)
data <- Boston
y <- data$medv
x <- as.data.frame(data[,5:8])
shapleyvalue(y,x)
}
\value{
The structure of the output is a datatable, with two rows:the unstandardized and standardized relative importance of each attributes using shapley value regression method.
}
