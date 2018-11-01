## In the following, two functions are defined which allow caching a matrix and its inverse. 
## If not previously cached by the user,  the inverse of the matrix is calculated, cached, and finally returned.

## The following function takes a matrix as input and return a list consisting of four
## functions that cache or return the input matrix or its inverse, respectively.

makeCacheMatrix <- function(x = matrix()) {
   inv_matrix <- NULL
   set <- function(y) {
      x <<- y     
      inv_matrix <<- NULL  
   }
   get <- function() x  
   setinverse <- function(inverse) inv_matrix <<- inverse   
   getinverse <- function() inv_matrix 
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## The following function takes the output of the function makeCacheMatrix() as input, 
## check if the inverse of the matrix has been already cached, if not, 
## the inverse is calculated, cached and then returned. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv_matrix <- x$getinverse()
   if(!is.null(inv_matrix)) {
      message("getting cached data")
      return(inv_matrix)
   }
   data <- x$get()
   inv_matrix <- solve(data, ...)
   x$setinverse(inv_matrix)
   inv_matrix
}
