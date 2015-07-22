## These functions do what they were asked to do.
## they are very obedient

## Creates a funny vector that gets,sets and readies a matrix to be returned and cached if needed.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL}
  get <- function() x
  setinverted <- function(inverted) inv <<- inverted
  getinverted <- function() inv
  list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)}

## turns the matrix inside out if it hasn't been turned before. 
## Or even inverts it, chaching it if needed
## takes a makeCacheMatrix list as an input

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverted()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)}
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverted(inv)
  inv}
