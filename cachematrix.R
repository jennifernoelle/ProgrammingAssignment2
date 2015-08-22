## R programming coursera - Programming Assignment 2 - Lexical Scoping
## This code creates two functions to cache the inverse of a matrix

## makeCacheMatrix - cache an input matrix, 'x' and its inverse
## Detailed: this fcn creates vector of functions
## which create a "special" matrix to set 
## and globally cache an input matrix and its inverse.
## The sub-fcns are later called via subsetting in cacheSolve

makeCacheMatrix <- function(x = matrix()) { 
    i <- NULL ## creates var for matrix inverse, sets to null
    set <- function(y) {
        x <<- y  ## allows you to reset matrix x to y, makes global/cached
        i <<- NULL ## makes empty inverse matrix, i global/cached
    }
    get <- function() x ##returns the matrix x
    setinverse <-function(inverse) i<-inverse ##lets you set inverse
    getinverse <-function() i ##stores/returns the value of inverse
    list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve - return a matrix that is the inverse of 'x'
## Detailed: retrieves the "special" matrix and its inverse, cached above
## if the inverse has not yet been computed and cached,
## this fcn computes and returns it

cacheSolve <- function(x) { ## input x, the special vector returned by parent fcn makeCacheMatrix
    i <- x$getinverse() ## get the inverse of cached matrix via getinverse(), created above
    if(!is.null(i)) {   ## if x's inverse was already set and cached, retrieve it
        message("getting cached data")
        return(i)
    }               ## if x's inverse not already cached...
    data <- x$get() ## retrieve cached data for matrix x from parent fcn
    i <- solve(data) ## inverts matrix x; assume matrix is invertible
    x$setinverse(i) ## sets/caches inversion to new value using parent fcn
    i ## returns inverted matrix, 'i'
}