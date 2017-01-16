## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## A function to create a 'special' cached matrix object from a matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
## A function to cache the original matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
## Cache the inverse matrix
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function

## A function which retrns the invers of a a matrix, either by 
## retrieving the solution from the cache, or solving for it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
## Test to see if result is cached and if so, return the cached matrix.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
## This part is run if no cached result is found, and solves for the inverse.
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


## Vector mean functions from original example


makeVector <- function(x = numeric()) {
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

cachemean <- function(x, ...) {
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
