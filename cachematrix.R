## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. This source file contains a pair of functions that cache the 
## inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
## "matrix object" is represented as a list with four following values:
## set - function to set the matrix;
## get - function to get the matrix;
## setinverse - function to set the cached inverse of the matrix;
## getinverse - function to get the cached inverse of the matrix.
## Function returns the special "matrix" object described above. This object
## can be used when cacheSolve() function is called as x argument.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_mat) inverse <<- inverse_mat
    getinverse <- function() inverse
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.
## x - "matrix" object can be obtained with makeCacheMatrix() function.
## Function returns the inverse of the matrix that was used for "matrix" object
## x creation.
## Important notes: There are two requirements for the input matrix:
##                  1. The matrix must be square 
##                     (same number of rows and columns);
##                  2. The determinant of the matrix must not be zero.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    
    if (length(dim(mat)) != 2 || dim(data)[1] != dim(data)[2]) {
        message("Error in cacheSolve(). The matrix must be square")
    } else if (det(data) == 0){
        message("Error in cacheSolve(). The determinant of the matrix must not be zero")
    }
    
    # To get the inverse, we use matrix equation: A(A^-1)=I,
    # where A is the input matrix A,
    #       I is the identity Matrix can be obtained as diag(dim(data)[1]),
    #       dim(A) is equal to dim(I).
    # We call solve(A, I) and get A^-1 as a result:
    inverse <- solve(data, diag(dim(data)[1]))
    x$setinverse(inverse)
    inverse
}
