## makeCacheMatrix() caches an inverse of a given matrix
## cacheSolve() returns the cached version of the inverse of a matrix
## where it exists otherwise calculates and returns the inverse

## Creates a set of functions for calculating the inverse, 
## caching and then retrieving a cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mat <<- solve
        getinverse <- function() mat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Retrives a cached inverse of a selected matrix 
## When the matrix has not been cacahed, calculates the inverse and 
## returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinverse()
        
        if(!is.null(mat)) {
        ##        print("getting cached inverse matrix")
                return(mat)
        }
        
        data <- x$get()
        ## print("getting uncached inverse matrix")
        mat <- solve(data, ...)
        x$setinverse(mat)
        mat        
}
