# Task 1
# Create a function which shows NA data positions in the vector.

# Variant 1
my_vector <- c(1, 2, 3, NA, NA)

NA.position <- function(x){
  result <- c()
  for (i in 1:length(x)) {
    if (is.na(x[i])) {
      result <- c(result, i)
    }
  }
  return(result)
}

NA.position(my_vector)

# Variant 2
NA.position <- function(x){    
  which(is.na(x))}



# Task 2 
# Create a counter function for NA data in vector.

my_vector_2 <- c(1, 2, 3, NA, NA)

NA.counter <- function(x){
    length(which(is.na(x)))
}

NA.counter(my_vector_2)



# Task 3
# Create a function that returns the sum of a positive elements
# of the vector

filtered.sum <- function(x){    
  return(sum(x[x > 0], na.rm = T))}


# Task 4
# Create a function that finds and removes outliers from the vector.
# Outliers are observations that deviate from the 1st or 3rd quartile
# by more than 1.5 * IQR (interquartile range).

outliers.rm <- function(x){
  iqr_data = IQR(x)*1.5
  qnts = quantile(x, probs = c(0.25, 0.75))
  new_qnts = c(qnts[1] - iqr_data, qnts[2] + iqr_data)
  
  return(x[!(x < new_qnts[1] | x > new_qnts[2])])
}

my_vector_3 <- c(-2.12, -5.42, 0.97, -0.21, -0.55, 0.04, -0.63,
                 -0.31, -0.43, 0.51, 0.53, -1.99, -0.55, -3.87,
                 0.74, 0.48, 0.17, -0.34, -2.9, 17.5, -1.26, 1.87,
                 -0.28, 0.73, -0.64, -125.74, -0.18, -0.04, -2.04, 1.42)

outliers.rm(my_vector_3)
