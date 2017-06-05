## This pair of function cache the inverse of a matrix.


#Part 1:
## "makeCacheMatrix" creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inver <<- solveMatrix
  getInverse <- function() inver
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

#Part 2:
## "cacheSolve" calculates the inverse of the special matrix returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setInverse(inver)
  inver      
}