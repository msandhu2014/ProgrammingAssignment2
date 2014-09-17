## There are two functions defined below. makeCacheMatrix can be used 
## to cache a matrix along with its inverse. cacheSolve returns the 
## inverse of the matrix.

## makeCacheMatrix - a function that returns a holder object for a matrix  
## and its inverse. The inverse is computed the first time and then cached 
## for future reading. The object also returns functions to perform basic 
## operations on the matrix, viz. get, set, getinv, setinv.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getinv <- function() inv
  setinv <- function(z) inv <<- z
  list( get = get, set = set, getinv = getinv, setinv = setinv )  
}


## cacheSolve - this function returns the inverse of the matrix stored
## in the passed in object. It checks if the inverse has already been 
## cached. If it has been cached, it returns the cached value, 
## otherwise it computes the inverse and caches it before returning 
## the value.

cacheSolve <- function(x) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
