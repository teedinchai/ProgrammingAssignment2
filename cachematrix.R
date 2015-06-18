## This program is used to create a pair of functions to cache the INVERSE 
## of a SQUARE matrix, and assume the input matrix is always INVERTIBLE.
## SQUARE matrix A* INVERSE(SQUARE matrix A) = INDENTITY matrix


## This function creates a special "matrix" object that can cache its inverse.
## I named this special matrix as "mi"(matrix inverse)
makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) mi <<- solve
  getInverse <- function() mi
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
} 

## This function computes the inverse of the special "matrix" (mi) returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (this means !is.null(mi) is TRUE)(and the matrix has not changed), 
## then the cacheSolve will retrieve the inverse of "mi" from the cache and
## return a matrix that is the inverse of "x".
cacheSolve <- function(x, ...) {
  mi <- x$getInverse()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setInverse(mi)
  mi                   ##return the inverse matrix of 'x'
}

##   -END-   ##