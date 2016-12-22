# x is a D-dimensional random variable (i.e., a Dx1 column vector) and each component of x is uniformly distributed between -5 and 5
get_x_input_vector <- function(vector_size){
  return (runif(vector_size, min=-5, max=5))
}

cost_func <- function(x_input_vector){
  D_length <- length(x_input_vector)
  numer <- exp(1)^(-0.5 * t(x_input_vector) %*% x_input_vector)
  denom <- (2 * pi)^(D_length/2)
  return (numer/denom)
}



in_x <- get_x_input_vector(2500)
head(in_x)
tail(in_x)
cost_func(in_x)
crudeMonteCarlo <- function(sampleSize, D){
}

for(i in c(1:10)){
  crudeMonteCarlo(i*1000, 1)
}
## 
