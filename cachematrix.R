## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.
## Below are two functions that are used to create a special object that stores a matrix
## and caches its inverse.

## NB For the scope of this assignment, we assume that the matrix is always squared.
## Notes are also based on this article: https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## This function creates a special "matrix" object that can cache its inverse.
## The steps followed are:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## Initialize 2 objects x and m. x is an empty matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # Now define the set function:
    set <- function(y) {
        x <<- y
        m <<- NULL # This clears `m` of any cached value caused by a previous run of chacheSolve(). So whenever x is reset, solve() is run again
    }
    # define the get function - R gets the value of x from the parent environment makeCacheMatrix()
    get <- function() x
    # set the Inverse
    setInverse <- function(solve) m <<- solve
    # Get the inverse
    getInverse <- function() m
    # Assgn all functions to a list and return to the parent environment
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve retrieves the inverse from the cache, using the solve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Get the current value of the inverse
    m <- x$getInverse()
    ## Get the cached value of m, if it was run previously
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## If m is NULL - i.e. data is not cached, then calculate the inverse
    data <- x$get()
    m <- solve(data, ...)
    # cache the result of the matrix
    x$setInverse(m)
    # return the result
    m
}

## Testing

# m1 <- makeCacheMatrix(matrix(1:4, 2, 2))
# cacheSolve(m1)
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
# Run again to see if the data is cached:
# > cacheSolve(m1)
#getting cached data
#    [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

