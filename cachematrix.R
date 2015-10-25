## These functions are able to cache potentially time-consuming computations, using the scoping rules of the R language.

## They do not only return the inverse of a “problem” matrix, but in case we want to calculate the inverse of this matrix again 
## recomputing the function is not necessary. The value of the first calculated inverse matrix was cached, if we need it again,
## it can be looked up in the cache rather than recomputed.

## The first function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
  set <- function (y){
    x <<- y
    m<<-NULL
    }
  get <- function()x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set=set, get=get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The second function calculates the inverse of the special "matrix".  However, it first checks to see if the inverse of 
## this matrix has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the matrix in the cache via the setsolve function.


cacheSolve <- function(x, ...) {
       m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
