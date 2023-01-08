# Task 1
# Use data, which illustrates impact of various fertilizers
# on the yield of peas.
# Goal: find out whether the simultaneous application of nitrogen (factor N)
# and phosphate (factor P) is essential.

# We will use variance analysis, which will test the influence
# of the nitrogen application factor (N), the influence of the phosphate
# application factor (P) and their interaction.
# Our two main hypothesis
# H0: different types of fertilizers has no difference in yield level.
# H1: there is a difference between fertilizers comparing the yield level.

# Step 1. Reading data
df <- npk
str(df)
View(df)
# We see that the key variables has not a factor type.

# Step 2. Changing types of variables
df$N = factor(df$N, labels = c('N not used', 'N used'))
df$P = factor(df$P, labels = c('P not used', 'P used'))
df$K = factor(df$K, labels = c('K not used', 'K used'))
# Now all the key variables have an appropriate type and we should check
# graphics of their interaction.

# Step 3. Looking at interaction of variables
# With basic function
boxplot(yield ~ (P + N), data = df)

# With ggplot2 library
library(ggplot2)
pd = position_dodge(0.1)
ggplot(df, aes(x = N, y = yield, color = P, group = P))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, pch=15, position = pd) +
  theme_bw()
# According to the boxplots
# there is a strong difference in using of nitrogen (factor N) (regardless of using
#phosphate (factor P)). Let's check the p-value.

# Step 4. Getting p-value of variance analysis
res <- aov(yield ~ P + N + P:N, data = df)
summary(res)
# The only strong result is for factor N – 0.0263 (F-value: 5.758).

# Let's add another one factor variable for additional check.
res2 <- aov(yield ~ P + N + K, data = df)
summary(res2)
# As above we see the only strong result for factor N – 0.0192 (F-value: 6.488).

# Conclusion: we can reject H0, but only for factor N:
# there is a difference in yield level for nitrogen fertilizer, which
# has the highest mean in the data.



# Task 2
# Goal: find out, which species of sepal differ in width statistically significant.
# We will use variance analysis with pairwise comparisons.
# H0: there is no significant difference in width of different species.
# H1: there is some significant difference in width of different species.

# Step 1. Reading data
df2 <- iris
str(df2)

# Step 2. Looking at interaction of variables
ggplot(df2, aes(x = Species, y = Sepal.Width)) +
  geom_boxplot()
# We see the difference between means of each group which
# can be statistically significant.

# Step 3. Looking at interaction of variables
res3 <- aov(Sepal.Width ~ Species, data = df2)
summary(res3)
# Species difference in width is statistically significant:
# p-value < 2e-16, F-value: 49.16.

# Step 4. Let’s complete pairwise comparisons to understand
# difference in species width
TukeyHSD(res3)
# We see statistically significant difference between all the species,
# combined in three pairs: ‘versicolor-setosa’ (p-value: 0.0000000),
# ‘virginica-setosa’ (p-value: 0.0000000), ‘virginica-versicolor’
# (p-value: 0.0087802).

# Conclusion: we van reject H0: all three sepal species has
# statistically significant difference in width.



# Task 3
# Data Set: temperature of several patients who are treated
# with different pills and by different doctors.
# Goal 1: to check the influence of the type of tablet (pill)
# on the temperature (temperature) taking into account the subject (patient).
# Goal 2: to check influence of such factors as doctor, pill
# and their interaction on temperature, considering both within-group variables
# (the same patient takes different pills and the same patient is treated
# by different doctors).
# H0: there no influence of mentioned factors on the temperature level.
# H1: there is some influence of mentioned factors on the temperature level.

# Step 1. Reading data
data <- read.csv('data_med.csv')
str(data)
View(data)

# Patient variable should have type ‘factor’, not ‘int’.
data$patient <- factor(data$patient)
str(data)

# Step 2. Checking interaction between temperature and pill
# We used ggplot boxplot function
library(ggplot2)
ggplot(data, aes(x = pill, y = temperature)) +
  geom_boxplot() +
  facet_grid(~patient)

# It seems there is no statistically significant interaction between
# two factors.

# Step 3. Variance analysis (taking into account the patient types).
result <- aov(temperature ~ pill + Error(patient/pill), data = data)
summary(result)
# We see, that there is no strong influence of the type of tablet (pill)
# on the temperature (temperature) taking into account the subject (patient).
# So we have to accept H0.

result2 <- aov(temperature ~ pill * doctor + Error(patient/(pill * doctor)),
               data = data)
summary(result2)
# The same result for such factors as doctor, pill
# and their interaction on temperature, considering both within-group variables.
# So we also have to accept H0.

# Conclusion: there is no strong influence of type of tablets or
# doctor factors on the temperature level.