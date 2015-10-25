## Put comments here that give an overall description of what your
## functions do
# On the one hand my makeCacheMatrix function creates an matrix object so 
# we can cache the inverse of a matrix. It is able to calculate it also. On the
# other hand, the cacheSolve function tries to get the cached inverse of the
# matrix, and in the case that it is not cached already, it calculates it and
# caches it int the matrix object.

## Write a short comment describing this function
# This function creates an matrix object that caches its inverse. It has some 
# functions that allows getting and setting its inverse, so it is possible to
# retrive it once it is cached

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## Write a short comment describing this function
# This function tries to retrive the cached inverse and returns it.
# In case that it is not cached, it calculates the inverse and caches
# it in the matrix object, and of course, it returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
