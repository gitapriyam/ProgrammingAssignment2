## There are two functions embedded here.

## makeCacheMatrix function creates a special "matrix" object that can cache the
## matrix and its inverse

## cacheSolve function takes the above special "matrix" object & verifies if the
## inverse of the matrix exists. if it doesn't, it creates the inverse and 
## stores the inverse with the above special "matrix". The inverse is 
## essentially cached with this special "matrix" object

## This makeCacheMatrix takes in a matrix object as a parameter and provides
## helper functions for accessing the matrix and its inverse and these
## free variables are stored in their internal environment.

makeCacheMatrix <- function(x = matrix()) {
    # the inverse is NULL
    i <- NULL
    
    # Set the matrix and default its inverse to NULL
    setMatrix <- function(y) {
        m <<- y
        i <<- NULL
    }
    # return the matrix passed to it
    getMatrix <- function() x
    
    #get the Inverse
    getInverse <- function() i
    
    # Set the inverse of the matrix
    setInverse <- function(z){
        i <<- z
    }
    # returns a labeled vector of functions setMatrix, getMatrix, 
    # setInverse and getInverse
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse= getInverse
         )
    
}


## This cacheSolve takes in a special matrix object makeCacheMatrix as a 
## parameter and verifies if the inverse exists. If it does,
## it returns the cached inverse. Otherwise, it computes the
## inverse, caches it and returns the inverse.


cacheSolve <- function(x, ...) {
    # get the inverse from the x parameter
    r <- x$getInverse()
    
    # verify if inverse exists
    if(!is.null(r)) {
        # if inverse exists then return the cached inverse
        message("getting cached data")
        return (r)
    }
    
    # inverse does not exist
    data <- x$getMatrix()
    
    #derive the inverse
    r <- solve(data, ...)
    
    #cache the inverse
    x$setInverse(r)
    
    #return the inverse
    r
}
