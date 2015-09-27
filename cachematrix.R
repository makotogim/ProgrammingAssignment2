## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## MakeCacheMatrix returns a list of functions called set, get, setInverse and getInverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inv = matrix()) m <<- inv
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## Write a short comment describing this function
## Cache Solve returns the inverse of a Matrix from cache. If there is not a matrix in the cache
## it calculates it,

cacheSolve <- function(x = matrix()) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
