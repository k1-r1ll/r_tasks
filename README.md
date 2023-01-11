# Statistical tasks accomplished with R

## 1. File ‘anova_tasks’
Contains several tasks.
### Task 1. Analyzing data, which illustrates impact of various fertilizers on the yield of peas
**Goal:** find out whether the simultaneous application of nitrogen (factor N) and phosphate (factor P) is essential.
### Task 2. Analyzing sepal data
**Goal:** find out, which species of sepal differ in width statistically significant (using variance analysis with pairwise comparisons).
### Task 3. Analyzing temperature of several patients who are treated with different pills and by different doctors.
**Goal 1:** to check the influence of the type of tablet (pill) on the temperature (temperature) taking into account the subject (patient).
**Goal 2:** to check influence of such factors as doctor, pill and their interaction on temperature, considering both within-group variables
(the same patient takes different pills and the same patient is treated by different doctors).

## 2. File ‘function_tasks’
Contains the following tasks.
### Task 1. Create a function which shows NA data positions in the vector.
### Task 2. Create a counter function for NA data in vector.
### Task 3. Create a function that returns the sum of a positive elements of the vector.
### Task 4. Create a function that finds and removes outliers from the vector.

## 3. File ‘correlation_tasks’
Contains the following tasks.
### Task 1. Function for correlation data
**Goal:** create a function ‘corr.calc’ which takes a two-column data-frame and returns vector with Pearson correlation coefficient and p-value.
### Task 2. Correlation between all pairs of numeric variables
**Goal:** create function ‘filtered.cor’ that takes data.frame with a free number of variables (both quantitative and any other types),
calculates Pearson correlation coefficient between all pairs of numeric variables and returns coefficient with the highest module.
# of numeric variables and returns coefficient with the highest module
### Task 3. Function for Spearman/Pearson coefficient with Shapiro-Wilk test check
**Goal:** create function ‘smart_cor’ for Spearman/Pearson coefficient with Shapiro-Wilk test pre-check if variable distributions are normal.
### Task 4. Linear regression for a subset
**Goal:** build a linear regression for diamonds data subset from the ggplot2 library.
### Task 5. Regression model with Pearson correlation coefficient check and updated dataframe
**Goal:** create function ‘regr.calc’ that updates dataframe with predicted values for dependent variable.
### Task 6. Linear regression data
**Goal:** get the intercept and slope for the dataframe.
### Task 7. Visualization for linear regression
**Goal:** create a scatterplot for the iris data.
