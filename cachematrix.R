## The functions in my Rscript will caclulate the inverse of a n x n matrix
## and store it in the cache.  If the inverse has already been calculated then
## rather than calculating it again it will be retreived from the cache


## This function creates a special matrix object which is a list containing a function to
## set the value of the matrix, get the value of the matrix, set the value of
## the inverse and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    inverse <<- NULL
    
  }
  
  get <- function() {x}
  
  setinverse <- function(solve) { inverse <<- solve}
  
  getinverse <- function() { inverse }
  
  list( get = get,
        setinverse = setinverse,
        getinverse = getinverse)
  
  
}


##  This function computes the inverse of the special matrix object returned by
## the makeCacheMatrix function above.  If the inverse already has been calculated
## then this function will retreive the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  
  if(!is.null(inverse)) {
    
    message("getting cached data")
    
    return(inverse)
    
  }
  

  
  data <- x$get()
  
  
  
  inverse <- solve(data, ...)
  

  
  
  x$setinverse(inverse)
  
  inverse
  
  
}
