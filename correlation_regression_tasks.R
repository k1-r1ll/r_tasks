# Task 1. Function for correlation data
# Goal: create a function ‘corr.calc’ which takes a two-column data-frame
# and returns vector with Pearson correlation coefficient and p-value.

corr.calc <- function(x){
  fit <- cor.test(x = x[[1]], y = x[[2]])
  return(c(fit$estimate[[1]], fit$p.value[1]))
}

corr.calc(mtcars[, c(1,5)])
corr.calc(iris[,1:2])

# ----------

# Task 2. Correlation between all pairs of numeric variables
# Goal: create function ‘filtered.cor’ that takes data.frame with
# a free number of variables (both quantitative and any other types),
# calculates Pearson correlation coefficient between all pairs
# of numeric variables and returns coefficient with the highest module
# (the function can return –0.9 if it is the largest module in result).

filtered.cor <- function(x){
  x.numeric <- x[,sapply(x, is.numeric)]
  cor_data <- cor(x.numeric)
  diag(cor_data) <- 0
  return(ifelse(abs(max(cor_data)) > abs(min(cor_data)),
                                         max(cor_data), min(cor_data)))
}

# ----------

# Task 3. Function for Spearman/Pearson coefficient with Shapiro-Wilk test check
# Goal: create function ‘smart_cor’ that takes a dataframe with two quantitative
# variables as argument. The function has to check with Shapiro-Wilk
# test if two distributions are normal. If at least one has result < 0.05,
# it should return Spearman correlation coefficient. In other case,
# the function returns Pearson correlation coefficient.

smart_cor <- function(x){
  p1 = shapiro.test(x[[1]])$p.value
  p2 = shapiro.test(x[[2]])$p.value
  if (p1 < 0.05 | p2 < 0.05) {
    return(cor.test(x[[1]], x[[2]],  method = "spearman")$estimate[[1]])
  } else {
    return(cor.test(x[[1]], x[[2]])$estimate[[1]])
  }
}

# ----------

# Task 4. Linear regression for a subset
# Goal: use diamonds data from the ggplot2 library.
# Only for class ‘Ideal’ (variable ‘cut’) with carats = 0.46 (variable ‘carat’)
# build a linear regression, where price is the dependent variable
# and depth is the predictor. Save the regression coefficients
# in variable ‘fit_coef’.

library(ggplot2)
df <- subset(diamonds, cut == 'Ideal' & carat == 0.46)
str(df)

fit <- lm(price ~ depth, df)
summary(fit)
fit_coef <- fit$coefficients
fit_coef

# ----------

# Task 5. Regression model with Pearson correlation coefficient check and
#         updated dataframe
# Goal: create function ‘regr.calc’ that receives a dataframe with two variables
# as argument. If they have strong Pearson correlation coefficient
# (p-value < 0.05), the function returns a regression model with the first
# variable as dependent variable and the second one as the independent one.
# Then it adds a new variable ‘fit’ to the basic dataframe with predicted
# values for dependent variable. The function returns the updated dataframe.
# If variables has Pearson correlation coefficient with p-value > 0.05,
# the function returns line ‘There is no sense in prediction’.

regr.calc <- function(x){
  my_cor <- cor.test(~ x[[1]] + x[[2]], x)
  if (my_cor$p.value > 0.05) {
    return('There is no sense in prediction')
  } else {
    model <- lm(x[[1]] ~ x[[2]], x)
    new <- data.frame(x[[2]])
    x$fit <- predict(model, new)
    return(x)
  }
}

# ----------

# Task 6. Linear regression data
# Goal: get the intercept and slope for the dataframe
# (where the first variable is dependent, the second one is independent).

df <- read.table('dataset_11508_12.txt', header = FALSE, sep = "")
result <- lm(V1 ~ V2, df)
summary(result)

# ----------

# Task 7. Visualization for linear regression
# Goal: create a scatterplot for the iris data.
# X-axis – variable ‘Sepal.Width’, y-axis – variable ‘Petal.Width’,
# dot color – variable ‘Species’. Also add a linear smoothing
# for each group of observations on the ‘Species’ variable.

library(ggplot2)

my_plot <- ggplot(iris, aes(Sepal.Width, Petal.Width, col=Species))+
  geom_point(size = 3)+
  geom_smooth(method = 'lm')
