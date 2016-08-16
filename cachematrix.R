## These functions are my submission for the
## Programming Assignment 2, R Programming Johns Hopkins Coursera course
## For a matrix x, cacheSolve(makeCacheMatrix(x)) returns the inverse of the matrix as requested

## Thanks for grading me!

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It creates a special "matrix",
## which is a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# The cacheSolve function calculates the inverse of the special "matrix"
# which was created with the makeCacheMatrix function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
# in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      m <- x$getmean()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      m
}
