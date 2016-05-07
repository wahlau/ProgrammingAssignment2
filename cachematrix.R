## Coursera Data Science Specialization - Johnis Hopkins University
## Module R-Programming
## Assignment 2: Lexical Scoping
## 
## Task: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. 
## Your assignment is to write a pair of functions that cache the inverse 
## of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
## the function will take in x as input, and can store an inverse of x as inv. 
## setter and getter are defined to store/set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
