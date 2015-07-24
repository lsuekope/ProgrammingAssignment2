## These two functions create a special matrix object and return the matrix inverse to the user.
## The computer may take a while to compute this values, so this functions also store the values,
## and once a matrix is "recognized" the value of the inverse is only returned.

## The makecacheMatrix creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
 
  i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## The cacheSolve function computes the inverse of the special "matrix" returned by the function above. 
## Once the user "apply" cacheSolve, the function checks if the inverse has already been calculated 
## and if the matrix remains the same, and then retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
  
}
