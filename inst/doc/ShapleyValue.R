## ----include = FALSE----------------------------------------------------------
library(tidyverse)
library(MASS)
library(kableExtra)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ShapleyValue)

## ----echo=TRUE----------------------------------------------------------------
data <- Boston
head(data) %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria")

## ----echo=TRUE, warning=FALSE-------------------------------------------------
y <- data$medv
x <- as.data.frame(data[,5:8])
value <- shapleyvalue(y,x)
value %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria")

