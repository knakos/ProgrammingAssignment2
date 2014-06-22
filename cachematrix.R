makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  org <- x$get()
  
  if(!is.null(m)) {
    
    message("getting cached data")
    
    ## this is to check that the matrix has not changed
    ## in lack of a better (way more cumbersome solution) i just show it...
    if(!is.null(org)){
      print(org)
    }
    
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}