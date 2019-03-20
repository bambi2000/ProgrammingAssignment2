# A pair of functions that cache the inverse of a matrix

# Creates a special object that stores a numeric matrix and cache's its inverse
# No check is performed to see if the matrix is square
makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
  # Initialize the cache
  
  set <- function(y) {
    x <<- y
    cache <<- NULL
  } # Method to save the matrix and clearing the cache in the parent environment
  
  get <- function() x
  # Method to retrieve the matrix
  
  setinverse <- function(inv) cache <<- inv
  # Method to save the inverse matrix to the cache in the parent environment
  
  getinverse <- function() cache
  # Method to retrieve the cached inverse matrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  # Return a list containing the 4 methods
}

# Return a matrix that is the inverse of 'x' by using a cached copy if possible
cacheSolve <- function(x, ...) {
  
  c <- x$getinverse()
  # Retrieve the contents of the cache
  
  if(!is.null(c)) {
    message("getting cached data")
    return(c)
  } # If the cache contains anything, return it
  
  data <- x$get()
  # Retrieve the matrix
  c <- solve(data, ...)
  # Calculate the inverse matrix
  x$setinverse(c)
  # Save the inverse matrix in the cache
  c
  # Return the inverse matrix
}