## makeCacheMatrix function defines an object that allows the storage and 
## caching of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve takes a makeCacheMatrix instance as an argument.  
## It returns the inverse of this instance, either from the cache if it has been
## calculated before, or calculates it and caches it in the makeCacheMatrix instance.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## For example: 
## Suppose we have a matrix 2x2 with values 1 to 4 as 

matrix1 <- matrix(1:4,ncol = 2, nrow = 2)

## Let's create a makeCacheMatrix instance as

matrix1Cached <- makeCacheMatrix(matrix1)

## To generate the inversed matrix let's call the cacheSolve function

cacheSolve(matrix1Cached)


