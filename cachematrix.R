##Matrix inversion is usually a costly computation and there may be 
##some benefit to caching the inverse of a matrix rather than compute 
##it repeatedly
##The following are a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setSolve <-function(solve) inv<<-solve
  getSolve<-function() inv
  list(set=set,get=get,
       setSolve=setSolve,
       getSolve=getSolve)
}

## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv<-x$getSolve()
  ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat, ...)
  x$setSolve(inv)
  inv
}
