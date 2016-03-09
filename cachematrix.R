## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix: Creates a matrix objetc that can cache its inverse
## cacheSolve: Returns the inverse of the matrix object created by makeCacheMatrix.
## if the inverse is already solved ( and the matrix has not changee) it retrieve the
## inverse from the cache data.


## Write a short comment describing this function
## This function gets a matrix object and returns a list that contains functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

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


## Write a short comment describing this function
## retrive the inverse of the matrix x, from cache if it has been calculated or making
## solve(x) operation if the matrix is changed or new.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve (data, ...)
        x$setinverse(m)
        m
}
