## This code allows you to 1) input a matrix, then 2) find 
## its inverse. It allows caching of the inverse so
## code needing to execute this in loops, for instance,
## can use the cached data if called.

## This block creates a matrix and allows different
## functions to set its value and store the
## inverse information. The indicator variable
## is initially set to NULL but gains a value
## once the inverse is computed.

makeCacheMatrix <- function(x = matrix()) {
  indicator <- NULL
  set <- function(y){
    x <<- y
    indicator <<- NULL
  }
  get <- function() x
  setinv <- function(inv) indicator <<-inv
  getinv <- function() indicator
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This block calculates the inverse of x and relays that
## information back to the variables in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  indicator <- x$getinv()
  if(!is.null(indicator)) {
    message("getting cached data")
    return(indicator)
  }
  data <- x$get()
  indicator <-solve(data,...)
  x$setinv(indicator)
  indicator
}
