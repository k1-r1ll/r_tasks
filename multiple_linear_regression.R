# Task 1. Create function for updating regression model
# Goal: update NA cells in dataset 
# Create function ‘fill_na’, that takes dataset with three features
# (2 numeric and 1 factor feature). Then it should build regression model
# for the third feature with the numeric ones ignoring NA data
# (in the last column). The use predicted values to fill NA data.
# The function returns updated dataset.

fill_na <- function(x){
  
  model <- lm(x[[3]] ~ x[[1]] + x[[2]], data = x)
  x$y_full <- ifelse(is.na(x[[3]]), predict(model, x), x[[3]])
  return(x)
  
}

# ----------

# Task 2. Different regression models check
# Goal: find combination with the highest R^2 adjusted.
# Choose a combination of ‘mpg’, ‘disp’, ‘drat’, ‘hp’ features
# for prediction of variable ‘wt’ with the highest R^2 adjusted
# in ‘mtcars’ dataset.

my_d <- mtcars[,c('wt', 'mpg', 'disp', 'drat', 'hp')]

model1 <- lm(wt ~ mpg + disp + drat + hp, data = my_d)
summary(model1)

model2 <- lm(wt ~ drat + hp + mpg, data = my_d)
summary(model2)

model3 <- lm(wt ~ hp + mpg, data = my_d)
summary(model3)

model4 <- lm(wt ~ mpg, data = my_d)
summary(model4)

model5 <- lm(wt ~ mpg + disp + hp, data = my_d)
summary(model5)

# ----------

# Task 3. Interaction t-value
# Goal: regression model, interaction t-value
# Use built-in ‘attitude’ dataset to predict the rating for the ‘complaints’
# and ‘critical’ variables. Find the t-value for the interaction of two factors.

new_data <- attitude
str(new_data)

model <- lm(rating ~ complaints * critical, new_data)
summary(model)

# ----------

# Task 4. Linear regression model with two predictors
# Goal: build a regression model
# For ‘mtcars’ dataset build a linear regression model that predicts
# fuel consumption (mpg) with car weight (wt) and transmission (modified am)
# and their interaction.

data <- mtcars
data$am <- factor(data$am, labels = c('Automatic', 'Manual'))
View(data)

model <- lm(mpg ~ wt * am, data)
summary(model)

# ----------

# Task 5. Regression vesualization
# Goal: to visualize interaction of two predictor
# Visualize the interaction of variables ‘wt’ and ‘am’; regression
# color should be built on ‘am’ feature (dataset ‘mtcars’).

library(ggplot2)
data <- mtcars
mtcars$am <- factor(mtcars$am)

my_plot <- ggplot(data, aes(x = wt, y = mpg, col = am)) +
  geom_smooth(method = 'lm')

my_plot
