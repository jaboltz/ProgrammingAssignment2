## cachematrix.R
## Programming Assignment #2
## Featuring:
## 1.  <<- operator 
## 2.  R solve function 

## This function creates a special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverseCache <- NULL
  set <- function(y) {
    x <<- y
    inverseCache <<- NULL
  }
  
  get <- function() x
  setInverseCache <- function(inverse) inverseCache <<- inverse
  getInverseCache <- function() inverseCache
  list(set=set, get=get, setInverseCache=setInverseCache, 
       getInverseCache=getInverseCache)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
  inverseCache <- x$getInverseCache()
  if(!is.null(inverseCache)) {
    message("getting cached data...")
    return(inverseCache)
  }
  
  data <- x$get()
  inverseCache <- solve(data)
  x$setInverseCache(inverseCache)
  inverseCache
}