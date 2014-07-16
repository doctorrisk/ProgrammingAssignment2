#These functions create a real square matrix and then the second returns the inverse.  
# The inverse is taken from the cache if it has previously been calculated.  Otherwise, it inverts it


#create a real square matrix where one can cache the inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setInv <- function(theInv) inv <<- theInv
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)

}


# get the inverse of x if it is cached.  Otherwise, calculate it and cache it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
