#####################################
#Imputation Using Linear Regression
#####################################

Ind <- function(t) {
  x <- dim(length(t))
  x[which(!is.na(t))] = 1
  x[which(is.na(t))] = 0
  return(x)
}

data$I <- Ind(data$y)
data

model <- lm(y ~ x, data = data)
summary(model)

#Imputing

for (i in 1:nrow(data))
{
  if (data&I[i] == 0)
  {
    data$y[i] = 9.74 + 1.590*data$x[i]
  }}

#####################################
#Imputation Using KNN
#####################################

library(VIM)

k_imputed_data <- kNN(data, variable = c("a", "b", k=10))

k_imputed_data



