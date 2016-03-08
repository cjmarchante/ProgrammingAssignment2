
## makeCacheMatrix
## returns a special vector that is a list containing functions to
##              set the matrix
##              get the matrix
##              set the inverse
##              get the inverse
##         The list is used as the input to cacheSolve() 

makeCacheMatrix <- function(x = matrix()) {

  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invm <<- inverse
  getInverse <- function() invm
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## cacheSolve
## this functions calculates the inverse of a matrix. 
## If the matrix inverse is already calculated it gets it from the cache, 
## otherwise it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getInverse()
  # if the inverse has already been calculated
  if (!is.null(invm)){
    # get it from the cache

    return(invm)
  }
  
  # if not, calculates the inverse 
  mat.data <- x$get()
  invm <- solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setInverse(invm)
  
  return(invm)
  
}

