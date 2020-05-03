## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        ## Initialize the inverse property
        i <- NULL
        
        ## Set the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Get the matrix
        get <- function() {
                x
        }
        
        ## Set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        ## Get the inverse of the matrix
        getInverse <- function() {
                i
        }
        
        ## Return a list of the functions outputs
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix" above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        
        ## Return the inverse only if already set
        if( !is.null(i) ) {
                message("getting cached data")
                return(i)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        i <- solve(data, ...) 
        
        ## Set the inverse to the object
        x$setInverse(i)
        
        ## Return the matrix
        i
}