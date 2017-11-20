## This file conatins 2 functions - one that takes a matrix 
## and prints one of the following : the matrix itsef 
## or its inverse based on the function calls get() or getInverse() and
## another function that checks if the value is already in the cache and
## returns the result either from the cahce or by computing. 


makeCacheMatrix <- function(x=matrix()){
  
    inverse <- NULL
    ## set function thaat sets the value of the matrix
    set <- function(y){
      x <<- y
      inverse <<- NULL
    }
    
    ## get function -> gets the value of the matrix - checking for cached values
    get <- function() x
    
    setInverse <- function() inverse <<- solve(x)
    getInverse <-  function() inverse
    
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
    ## x <- as.list(x)
    
    ## Gets the inverse of the matrix and stores it in inverse.
    inverse <- x$getInverse()
    
    ## Condition to check if the 'inverse' is null
    ## if not NULL, then the value is alreay cached, so return exisitng value.
    if(!is.null(inverse))
    {
      message("getting cached data")
      return (inverse)
    }
    
    ## Inverse matrix value is calculated using solve() function
    ## and stored in cahce for further reuseage.
    matrixData <- x$get()
    inverse <- solve(matrixData, ...)
    x$setInverse(inverse)
    inverse
}