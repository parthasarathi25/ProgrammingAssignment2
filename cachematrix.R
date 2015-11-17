## Cache the Matrix Inverse

## special object created, stores the matrix and  cache the matrix inverse
## This functions help to do that

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(b) {
    x <<- b
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(minv) i <<- minv
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## It compute inverse of matrix that create by above function, 
## If inverse already calculated, then it collects inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("cache data.. recovered..")
    return(i)
  }
  z <- x$get()
  i <- solve(z, ...)
  x$setInverse(i)
  i
}
