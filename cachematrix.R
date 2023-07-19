#Carla Minakata
#18/07/2023
#Programming Assignment 2



#First we need to generate a function to create a cacheable matrix
makeCacheMatrix <- function(a) {
  # Variable to store the inverse of the matrix
  matrix_inverse <- NULL
  
  # Function to get the matrix
  get <- function() a
  
  # function tu set the matriz and calculate its inverse
  set <- function(b) {
    a <<- b
    matrix_inverse <<- NULL
  }
  
  #Function to obtain the inverse of the matrix
  get_Matrix_Inverse <- function() {
    
      if (!is.null(matrix_inverse)) {
        # If the inverse is already calculated, it is retrieved from the cache
        message("---One moment, we are retrieving the inverse of the matrix---")
        return(matrix_inverse)
    }
    
    # If the inverse is not cached, it is computed and stored
    message("---Calculating the inverse of the matrix---")
    matrix_inverse <- solve(a)
    return(matrix_inverse)
  }
  
  #Returns a list with the defined functions
  return(list(get = get, 
              set = set, 
              get_Matrix_Inverse = get_Matrix_Inverse))
}

# Function to calculate the inverse of the cached matrix
cacheSolve <- function(a) {
  # Here we get the inverse of the matrix
  matrix_inverse <- a$get_Matrix_Inverse()
  
  # Return the inverse
  return(matrix_inverse)
}
