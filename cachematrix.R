#
# This code forms the submission for the second programming assignment
# It demonstrates an R function that is able to cache potentially time-consuming computations.
#
# In this example, calculating the inverse of a matrix is typically a fast operation. 
# However, for a very large matrix, it may take too long to compute the inverse, especially if it has to be computed repeatedly (e.g. in a loop).
# If the contents of a matrix are not changing, it may make sense to cache the value of the inverse so that it can be looked up in the cache 
# rather than recomputed.
#
# In this Programming Assignment we are taking advantage of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object.
#
# Sample run : (see http://www.mathwords.com/i/inverse_of_a_matrix.htm for source of matrix)
#
# > source ("cachematrix.R")
# > x <- rbind ( c(4, 3), c(3, 2) )
# > m = makeCacheMatrix(x)
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4

#
# This is a setup function that creates a list containing the following four functions:
#   1. Set the input value: a matrix
#   2. Retrieve the input value
#   3. Set the output value: the inverse of the matrix in 1.
#   4. Get the output value (null if not previously calculated, or input matrix has changed) 
#
makeCacheMatrix <- function(x = matrix()) {
  #
  # s is the result of solving the input matrix
  # Initialise to null
  #
  s <- NULL
  #
  # x is the input matrix
  # Set to the new input value and clear the cached solve value
  #
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  #
  # This function returns the input matrix
  #
  get <- function() x
  #
  # This function sets the value of the inverse of the input matrix
  #
  setsolve <- function(solve) s <<- solve
  #
  # This function returns the value of the inverse of the input matrix
  #
  getsolve <- function() s
  #
  # This is the list of available functions that we return when makeCacheMatrix is executed
  #
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#
# This is an execution function that uses makeCacheMatrix and returns the inverse of a supplied matrix.
# It checks if the inverse has already been computed and stored. If so, it returns the previous result
# thus avoiding performing the calculation again.  If no cached result is found, it computes the inverse 
# and sets the value in the cache.
#
# Assumption: The matrix is always invertible
#
cacheSolve <- function(x, ...) {
  #
  # Retrieve the current cached result
  #
  s <- x$getsolve()
  #
  # A value other than null indicates that a cached result was received, so
  # return that to the caller
  #
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  #
  # No cached result, so get the input data
  #
  data <- x$get()
  #
  # Calculate the inverse of the matrix
  #
  s <- solve(data, ...)
  #
  # Store the result as the cached value
  #
  x$setsolve(s)
  #
  # Return the result to the caller
  #
  s
}
