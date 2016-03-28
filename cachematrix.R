# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than computing it repeatedly 

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #create a variable called inv_value
  inv_value <- NULL
  
  set <- function(y) {
    x <<- y
    inv_value <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) inv_value <<- inverse
  getInverse <- function() inv_value
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  
  inv_value <- x$getInverse()
  
  #check if the variable value exists
  if (!is.null(inv_value)) {
    message("getting cached data")
    return(inv_value)
  }
  
  matrix_value <- x$get()
  #check if the 2 values are the same
  inv_value <- solve(matrix_value, ...)
  x$setInverse(inv_value)
  
  #return the variable
  inv_value
  
}
