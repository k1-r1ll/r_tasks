# Task 1. Building logistic regression model
# Goal: build a logistic regression model for ‘mtcars’ dataset.
# Dependent variable is ‘am’ and predictors are: ‘disp’, ‘vs’, ‘mpg’.

df <- mtcars
str(mtcars)

df$am <- factor(df$am, labels = c('Automatic', 'Manual'))

library(ggplot2)
ggplot(df, aes(disp, mpg, col = vs))+
  geom_point(size = 5)+
  facet_grid(.~am)+
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=5,face="bold"))

model <- glm(am ~ disp + mpg + vs, df, family = "binomial")
model

log_coef <- model$coefficients
log_coef

# ----------

# Task 2. Presenting variable differences
# Goal: build a boxplot that shows differences for feature ‘len’ according
# to the dosing and product variables (dataset ‘ToothGrowth.’).
# X should stand for ‘supp’, Y should stands for ‘len’, and color – for ‘dose’.

library(ggplot2)
df <- ToothGrowth
df$dose <- as.factor(df$dose)

obj <- ggplot(df, aes(supp, len, fill = dose))+
  geom_boxplot()
obj

# ----------

# Task 3. Correlation check function
# Goal: create a function that returns variable names with the highest
# correlation coefficient from dataset. The dataset has arbitrary number 
# of numeric variables.

high.corr <- function(x){
  x.numeric <- x[,sapply(x, is.numeric)]
  cor_data <- cor(x.numeric)
  diag(cor_data) <- 0
  max_corr <- ifelse(abs(max(cor_data)) > abs(min(cor_data)),
                max(cor_data), min(cor_data))
  result <- which(cor_data == max_corr, arr.ind = TRUE)

  return(row.names(result))
}

high.corr(swiss)

# ----------

# Task 4. Prediction with logistic regression
# Goal: predict university admission results.
# Use dataset with american schools information to build a model for
# feature ‘admit’ (filter not NA rows). Then use it to predict results
# for all the NA rows. Use features ‘rank’ and ‘gpa’ as predictors.
# Admission is considered successful with probability >= 0.4.
# Show the number of admitted students.

# Step 1. Read the data
my_data <- read.csv('data.csv')
str(my_data)
View(my_data)

# Step 2. Changing variables type
my_data$rank <- as.factor(my_data$rank)
my_data$admit <- as.factor(my_data$admit)
str(my_data)

# Step 3. Creating NA and not-NA datasets
act_data <- subset(my_data, !is.na(admit))
emt_data <- subset(my_data, is.na(admit))
View(act_data)
View(emt_data)

# Step 4. Building a model and checking coefficients
model <- glm(admit ~ rank * gpa, act_data, family = "binomial")
summary(model)
exp(model$coefficients)

# Step 5. Prediction of results for NA dataset
emt_data$admit <- predict(model, newdata = emt_data, type = "response")
View(emt_data)

# Step 6. Creating a new feature, that shows admission status
emt_data$res <- factor(ifelse(emt_data$admit > 0.4, 1, 0), labels = c('N', 'Y'))
View(emt_data)
length(emt_data[emt_data$res=='Y',1])
