## These functions are intended to cache the inverse of a matrix using the two functions shown below
## 

## this function will create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 d <- NULL
  set <- function(y) {
    x <<- y
    d <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) d <<- inverse
  getinverse <- function() d
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##This functioon computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should return the inverse from the cache.


cacheSolve <- function(x, ...) {
    d <- x$getinverse()
+   if(!is.null(d)) {
+     message("getting cached data")
+     return(d)
+   }
+   data <- x$get(',')
+   d <- solve(data, ...)
+   x$setinverse(d)
+   d    
}
