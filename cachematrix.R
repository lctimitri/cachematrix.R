## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function can transfer the matrix class (x) to a class 
## including 4 member functions 1. set (can set the matrix value)
##                              2. get (can obtain the matrix value)
##                              3. get_inverse (can set the inverse matrix value)
##                              4. get_inverse (can get the inverse matrix value)

makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL
    set <- function(y) {
        x <<- y
        x_inverse<<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) x_inverse <<- inverse
    get_inverse <- function() x_inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## Write a short comment describing this function
## this function can compute the CacheMatrix class (x)'s inverse matrix(x_inverse) 
## the core is to use solve function to find the inverse matrix' value which can
## be returned by the member function get_inverse() in CacheMatrix class
## if the inverse matrix can't be calculated correctly cacheSolve will return error

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inverse <- x$get_inverse()
    if(!is.null(x_inverse)) {
        message("getting cached data")
        return(x_inverse)
    }
    data <- x$get()
    x_inverse <- solve(data, ...)
    x$set_inverse(x_inverse)
    x_inverse
}