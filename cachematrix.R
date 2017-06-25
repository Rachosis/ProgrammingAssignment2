
## makeCacheMatrix is a function that creates a special object  ("matrix" Object) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" created by the function makeCacheMatrix
## If the inverse was already calculated and the matrix  doesn't change,
## then this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

##-----Program test-----

##m <- matrix(rnorm(20),4,4)
  ## mbis <- makeCacheMatrix(m)
  ## cacheSolve(mbis)
  
##             [,1]      [,2]       [,3]       [,4]
##[1,] -0.319291067  0.509917 -0.2781291  1.1676236
##[2,] -0.966439032 -1.400884  0.5099959  0.6783028
##[3,] -1.025421633 -1.156100  0.7580191  0.9035085
##[4,]  0.001457165  1.336570 -0.4525296 -0.5989953
