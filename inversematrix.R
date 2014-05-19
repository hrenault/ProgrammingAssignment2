## Coursera R-programming - assignment 2
## this files contains 2 functions and one example of how to run them.

## This first function creates a special "matrix" object that can cache its inverse 
## It takes a matrix as argument and store it in cache. Then, it returns a list
## of 4 functions to "work" on it: set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
        ## initialize the inv variable 
        inv <- NULL                   
        ## create the set function that will cache the matrix passed in arg.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## create the get function that can retrieve the matrix from cache
        get <- function() x
        
        ## create a setinverse function. This function will calculate the inverse 
        ## of a matrix to be passed as argument, using the function solve. 
        ## the result is saved in cache
        setinverse <- function(solve) inv <<- solve
        ## create a getinverse function. 
        ## It will retrieve the result from the setinverse from cache 
        getinverse <- function() inv
        
        ## Build a list of 4 above function to be returned by makeCacheMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This second function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## retrieve the inverse of the matrix stored in cache. 
        inv <- x$getinverse()
        
        ## if the inv variable is not NULL, return the value stored in cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if the makeCacheMatrix had not been run yet, the cache was empty 
        ## We retrieve the matrix from cache.
        data <- x$get()
        ## then we calculate the inverse and cache it before returning it as result.
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Tested Example:-------------
> mat <- matrix(c(1,1, 0, 1), 2,2)
> mymatrix <- makeCacheMatrix(mat)
> cacheSolve(mymatrix)
[,1] [,2]
[1,]    1    0
[2,]   -1    1
> cacheSolve(mymatrix)
getting cached data
[,1] [,2]
[1,]    1    0
[2,]   -1    1
## end of tested example -----------
