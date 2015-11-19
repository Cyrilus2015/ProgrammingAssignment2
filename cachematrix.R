## The pattern of matrix inversing with caching

## This function returns the special list with matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInvMatr <- function(solve) m <<- solve
      getInvMatr <- function() m
      list(set = set, get = get,
           setInvMatr = setInvMatr,
           getInvMatr = getInvMatr)
}


## This function calculates inversed matrix using the cache

cacheSolve <- function(x, ...) {
      m <- x$getInvMatr()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInvMatr(m)
      m
}
