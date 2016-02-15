## The following two functions can cache and compute the 
## inverse of a matrix;

## This function creates a special object: that is a matrix, which caches
## its inverse;

makeCacheMatrix <- function(x = matrix()) {
   inverse_m <- NULL
   set<-function(y)
   {
      x <<- y
      inverse_m <<- NULL
   }
   get<-function()
      x
   setinverse <- function(inverse)
      inverse_m <<- inverse
   getinverse <- function()
      inverse_m
   list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function calcuates the inverse of the special object("matrix")
## returned by "makeCacheMatrix" above. If the inverse has already been
## calculated, this function should get the inverse from the cache;

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inverse_m <- x$getinverse()
   if(!is.null(inverse_m))
   {
      message("getting cached data")
      return(inverse_m)
   }
   data <- x$get()
   inverse_m <- solve(data,...)
   x$setinverse(inverse_m)
   inverse_m
}
