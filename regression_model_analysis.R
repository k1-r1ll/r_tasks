# Task 1
# With function 'step' find an appropriate model for prediction of
# 'rating' feature (dataset 'attitude'). Also compare this model with
# the previous one with anova.

model_full <- lm(rating ~ ., data = attitude)
model_null <- lm(rating ~ 1, data = attitude)

ideal_model <- step(model_null, scope = list(lower = model_null, upper = model_full),
     direction = 'forward')

summary(ideal_model)

anova(model_full, ideal_model)

# ----------

# Task 2
# Predict 'sr' feature with all the other ones (dataset 'LifeCycleSavings').
# Use all the main variable with their possible interactions (up to level 2).

data <- LifeCycleSavings
View(data)
str(data)

model <- lm(sr ~ (pop15 + pop75 + dpi + ddpi)^2, data)
summary(model)

# ----------

# Task 3
# Create function that returns standardized coefficients for regression
# model. Dataframe has two quantitative variables: the first one is dependent,
# the second one is predictor.

# Variant_1
beta.coef <- function(x){
  data_1 = scale(x[[1]])
  data_2 = scale(x[[2]])
  result = lm(data_1 ~ data_2)
  
  return(c(result[[1]][[1]], result[[1]][[2]]))
}

# Variant_2
beta.coef <- function(x){    
  x <-scale(x)    
  return(lm(x[,1] ~ x[,2])$coefficients)}

beta.coef(mtcars[,c(1,3)])

# ----------

# Task 4
# Create function ‘normality.test’, that check each variable of dataframe
# for normal distribution (with 'shapiro.test' function).
# Function returns vector with variable names and their p-values.

# Variant_1
normality.test  <- function(x){
  
  result <- vector()
  for (name in colnames(x)){
    result <- c(shapiro.test(c(x[name])[[1]])$p.value, result)
  }
  names(result) <- colnames(x)
  return(result)

}

# Variant_2
normality.test  <- function(x){    
  return(sapply(x, FUN =  shapiro.test)['p.value',])}

# ----------

# Task 5
# Create function 'resid.norm' that check distribution of model residuals.
# The function should return histogram with red fill if p < 0.05 and
# with the green one in the other case.

library(ggplot2)

data <- lm(mpg ~ disp, mtcars)

resid.norm <- function(fit){
  
  df <- data.frame(fit$model)
  residuals <- fit$resid
  if (shapiro.test(residuals)$p < 0.05) {
    
    plot <- ggplot(df, aes(x = residuals)) + 
      geom_histogram(binwidth = 1, fill = 'red', col = 'black')
    
  } else {
    
    plot <- ggplot(df, aes(x = residuals)) + 
      geom_histogram(binwidth = 1, fill = 'green', col = 'black')
  
  }
  
  return(plot)
}

resid.norm(data)

# ----------

# Task 6
# Create function 'high.corr' that returns vector with names of two
# variables that have the highest correlation coefficient in the
# dataset.

high.corr <- function(x){
  x.numeric <- x[,sapply(x, is.numeric)]
  cor_data <- cor(x.numeric)
  diag(cor_data) <- 0
  max_corr <- ifelse(abs(max(cor_data)) > abs(min(cor_data)),
                     max(cor_data), min(cor_data))
  result <- which(cor_data == max_corr, arr.ind = TRUE)
  
  return(row.names(result))
}
