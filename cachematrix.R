## The primary purpose of the following pair of functions is to
## cache the inverse of a matrix. Such caching prevents repeated
## computations and allows us to take advantage of R's lexical 
## scoping rules. Note, caching refers to the manner in which 
## a computer stores data in its memory that enables us to access
## the same piece of data, faster.

## makeCacheMatrix takes in a matrix object by default and returns 
## a list of functions that first mutate the object and then access
## the mutated object stored in the computer's memory.
## Assumption: x is an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
          i <- NULL

          set <- function(y) {
            x <<- y
            i <<- NULL
          }

          get <- function() x
          setinverse <- function(inverse) i <<- inverse
          getinverse <- function() i

          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)

}


## cacheSolve takes in the return value of makeCacheMatrix
## as its initial argument and retrieves the inverse of the 
## original atomic vector from the computer's memory, i.e, the
## cached inverse
## Note: Simply passing an atomic vector as an argument for
## cacheSolve WILL raise an error

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
  
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        
}
