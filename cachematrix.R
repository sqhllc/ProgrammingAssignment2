cachematrix <- function(data) { 
## This function creates a special "matrix" object and computes the inverse solution. If
## an inverse solution doesn't already exists, function will create inverse solution.
## Otherwise, function will retrieve solution from cache memory.
## This function assumes that all matrix supplied will be invertible.
## For example, an invertible matrix is  data <-diag(x=10,5,5)

## The function makeCacheMatrix creates a special matrix object that can cache its inverse
  
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # initialize inverse matrix variable

  ## Initialize cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Define functions for accessing the cache
  get <- function () x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  
  ## Packaging accessor functions for the cache
  list (set=set, get=get, setinverse = setinverse, getinverse = getinverse)
  }

## The following function either computes the inverse or retrieves the inverse solution
## from cache

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) { 
    message ("getting cache data")
    return(m)
  }
  data <- x$get()  # retrieve the original matrix
  m <- solve(data) # compute the inverse of the original matrix
  x$setinverse(m)  # store the inverse in cache
  m
}

## Demonstration of caching of matrix inverse
cm <- makeCacheMatrix(data)
a<-cacheSolve(cm) 
print(a)
b<-cacheSolve(cm) 
print(b)
}