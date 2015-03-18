## Put comments here that give an overall description of what your
## functions do

## These functions implement a cacheable version of a matrix, in the 
## sense that whenever the user needs to invert the matrix, the system
## checks for the existence of a cached version. This is useful when 
## dealing with very large matrices, which are computationally expensive
## to invert.

## Write a short comment describing this function

## makeCacheMatrix() defines a list structure that stores the matrix and
## its inverse, and provides set- and get-type functions for both the 
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function() {
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m_inv <<- inverse
    getinverse <- function() m_inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve() calculates the inverse of a matrix, whenever a cached
## version is not available. Otherwise, it saves time and computational
## resources, and readily returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m_inv <- x$getinverse()
    if (!is.null(m_inv)) {
        message("Getting cached data")
        return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data)
    x$setinverse(m_inv)
    m_inv
}
