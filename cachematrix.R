## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() and cacheSolve() are designed to compute 
## the inverse of a matrix and store it in a cache so as to prevent
## un-necessary re-computation

## Write a short comment describing this function
## This function initilializes the matrix to be cached and 
## then invert it. Finally it is cached.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setmatrix <- function(solve) m <<- solve
  
  getmatrix <- function() m
  
  list(set=set, get=get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function
## THis function detects whether a cached version of the matrix
## already exists and print it. Else, it inversts it and print it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
