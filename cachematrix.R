 Testing the program
# create a matrix > x <- matrix(rnorm(25), nrow = 5)          
# create a special matrix > cx <- makeCacheMatrix(x)
# return the matrix > cx$get()  
# return the inverse > cacheSolve(cx) 
# call the inverse matrix again > cacheSolve(cx)
# should see a message that matrix is being retrieved from the cache

# 1st part of program - create steps to set and get values for a created matrix
makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  inv <- NULL
  
  # Set the value for the matrix
  # <<- is an operator that can be used to assing a value to an object
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Get the value for the matrix
  get <- function() x
  # Set the value for the inverse
  setinv <- function(inverse) inv <<- inverse
  # Get the value for the inverse
  getinv <- function() inv
  # Return the matrix with new defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
# 2nd part of program - calculate the inverse of the matrix
# If inverse of the matrix is calculated before, it should returned the inverse matrix from cache
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  # If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse is not yet calculated, calculate it with the solve function
  data <- x$get()
  inv <- solve(data, ...)
  # Cache the inverse
  x$setinv(inv)
  # Return it
  inv
}
