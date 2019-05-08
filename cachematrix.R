## These are two functions that create a spetial object that stores a matrix ant caches 
## inversed matrix for it

## makeCacheMatrix creates a "special object"

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setSolve <- function(solve) m <<- solve
getSolve <- function() m
list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}


## cacheSolve returnes inverted matrix for the matrix created by makeCacheMatrix, if it wasn't
## calculated before. If it was already calculated, then it gets it from cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
