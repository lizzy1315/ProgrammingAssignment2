#this function creates the inverse of a matrix
#it caches that inverse so that if it has already been computed
#you can use the cached inverse instead of recalculating it
#to use this function first create an inversable matrix 
#example inversable matrix: matrix1 <- matrix(c(4,3,3,2), nrow=2, ncol=2)
#call this function and assign the answer to an object
#example matrix1_cache <- makeCacheMatrix(matrix1)
#then call the cacheSolve function on that object
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## this function can be used to cache the results of an inverse of a matrix
# this function is using the solve function to find the inverse
# first this function checks to see if the inverse is already cached in which case it returns the cache
# if it has not been cached it computes the inverse and stores it in the cache
# to use this function first execute makeCacheMatrix on your matrix and assign the output to an object
#then call this function on that object
#no other arguments are needed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
  }
}
