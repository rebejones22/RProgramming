##This program uses a pair of functions to cache and compute the inverse of a matrix

## Functions used to create a special matric object and cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <-NULL
  set <- function(x) {
    mmm <<-x;
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  setinverse <-function(inv2) inverse <<-inv2;
  getinverse <-function() return (inverse);
  return(list(set=set, get=get, setinverse=setinverse, getinverse=getinverse))
}


## COmputing the inverse of matrix using the makeCacheMatrix
## If inverse exists then cacheSolve will retrieve inverse from cache

cacheSolve <- function(mmm, ...) {
        inverse <- mmm$getinverse()
        if(!is.null(inverse)){
          message("getting cached data")
          return(inverse)
        }
        data <-mmm$get()
        inverse <-solve(data, ...)
        mmm$setinverse(inverse)
        return (inverse)
}
