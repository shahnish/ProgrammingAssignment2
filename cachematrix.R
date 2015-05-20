## Put comments here that give an overall description of what your
## functions do

## There are two functions in this file.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.


## Write a short comment describing this function
## 1. makeCacheMatrix
##      This has list of functions which does several things. The return is list of these four functions
##      If function is called with matrix, it will set it and set the inverse to null
##      The four functions perform
##      a. get
##          returns the matrix object
##      b. set
##          sets new matrix object and resets inverse to null
##      c. getinverse
##          returns inverse of the matrix
##      d. setinverse
##          sets the inverse of matrix to provided value
makeCacheMatrix <- function(x = matrix()) {
    
    ## sets inverse of matrix to NULL
    inverse <- NULL
    
    ## get function which returns the matrix
    get <- function() {
        x
    }
    
    ## set function which sets matrix to provided value and resets inverse. Note that it sets variables in parent environment using <<-
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## getinverse function returns the inverse of matrix
    getinverse <- function() {
        inverse
    }
    
    ## setinverse function sets the inverse value to the provided  value. Again note the <<- operator 
    setinverse <- function(inv) {
        inverse <<- inv
    }
    
    ## last statement will be return. It assigns function names to variables
    list (get = get, set = set, getinverse = getinverse, setinverse = setinverse)
    
}


## Write a short comment describing this function
## This function takes the function vector list containing matrix and then gets inverse
## If returned inverse is NULL then it calculates inverse and sets it
## If returned inverse is Not NULL then it uses the cached version.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## get the inverse
    im <- x$getinverse()
    ## if inverse is not null then use cached version
    if (!is.null(im))  {
        message("Using cached data")
        return(im)
        
    }
    
    ## control will come here only if inverse is null. In that case, set the inverse
    m <- x$get()   ## get the matrix
    im <- solve(m, ...)  ## get inverse
    x$setinverse(im)   ## sets the inverse so can be used later
    
    return (im)
}



## How to use
## source("cachematrix.R")
## m <- matrix(c(25,30,35,45,50,3,65,70,75), nrow=3, byrow=TRUE)
## mymatrix <- makeCacheMatrix(m)
## cacheSolve(mymatrix)   ## This will return calculated inverse
## cacheSolve(mymatrix)  ## This will return value from cache
