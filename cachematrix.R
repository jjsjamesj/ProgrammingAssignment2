## makeCacheMatrix takes as argument a square invertible matrix
## and returns a list of getter and setter methods for the matrix
## and its inverse.

## code of the form 'X<<-Y' assigns to X in the parent environment

makeCacheMatrix <- function(x =matrix() ) {
  xinverse <- NULL
  set <- function(y) {
    x <<- y                     
    xinverse <<- NULL           
  }
  get <- function() x
  setxinverse <- function(inverse){xinverse <<- inverse} 
  getxinverse <- function() xinverse
  list(set = set, get = get,
       setxinverse = setxinverse,
       getxinverse = getxinverse)
}

## cacheSolve takes as argument a list, such as type returned by 
## makeCacheMatrix. It then checks whether or not the inverse is already
## cached. If the inverse is already cached, it returns the cached inverse
## If the inverse is not already cached, then it calls solve()
## and the inverse setter method, and then returns the inverse. 


cacheSolve <- function(x, ...) {
  Xinverse <- x$getxinverse()
  if(!is.null(Xinverse)) {
    message("getting cached data")
    return(Xinverse)
  }
  data <- x$get()
  Xinverse <- solve(data, ...)
  x$setxinverse(Xinverse)
  Xinverse
        
}
