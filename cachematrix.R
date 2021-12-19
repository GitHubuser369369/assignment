## Put comments here that give an overall description of what your
## functions do
#: Caching the Inverse of a Matrix


## Write a short comment describing this function
#: a  "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
              N <- NULL
            fun <- function(y){
              x <<- y
              N <<- NULL
  }
  uti <- function()x
  setInverse <- function(inverse) N <<- inverse
  getInverse <- function() N 
  list(fun = fun, uti = uti, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Write a short comment describing this function
#: It computes the inverse of the special "matrix" returned by `makeCacheMatrix` above

cacheSolve <- function(x, ...) {
  N <- x$getInverse()
  if(!is.null(N)){
    message("cache")
    return(N)
  }
  Cs <- x$uti()
  N <- solve(Cs,...)
  x$setInverse(N)
  N
}
