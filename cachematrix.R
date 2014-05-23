## Two functions for caching a matrix and its inverse since this can be 
## a resource intensive operation and is often done repeatedly.
## The matrix must be created with the special makeCacheMatrix function

## Create a special version of matrix that makes a matrix that can be cached
## includes special methods for getting and setting the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## set up the variables x and m to hold the matrix and its inverse
        ## the matrix is set to the matrix passed in, the inverse is NULL initially
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## create the methods for the special matrix handling
        
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## If the inverse of the matrix already exists, return it
## Otherwise, create the inverse, cache it for future use, and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Find out if the inverse has already been created
        
        m <- x$getinverse()
        if(!is.null(m)) {
                
                ## The inverse exists, just return it
                
                message("getting cached data")
                return(m)
        }
        
        ## The inverse hasn't been created, use the special method to 
        ## get the matrix, use the standard solve function to get the inverse
        ## use the special method to set the inverse for future use
        ## return the inverse
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
