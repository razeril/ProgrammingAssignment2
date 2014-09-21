## Two functions (makeCacheMatrix and cacheSolve) are used to create a matrix with
## the capability of caching the inverse matrix results.
## Note: it is assumed the matrix is always invertible

## makeCacheMatrix function takes a matrix (x) as an argument
## with internal functions for set, get, setInverse and getInverse
## returns a list of functions
makeCacheMatrix <- function(x = matrix()) {
    ## im variable (inverse matrix) initialized as NULL
    im <- NULL
    
    ## Set function (e.g. $set(matrix))
    set <- function(y){
          x <<- y  ## set x (matrix) through y arg and sets im to NULL
          im <<- NULL ## Set the im variable (inverse matrix) to NULL
    }
    
    ## get function returns matrix
    get <- function() x
    
    ## setInverse function
    ## Works the same as solve function, returning inverse results to im
    setInverse <- function(solve) im <<- solve
    
    ## getInverse function
    ## returns the value of im (the inverted matrix)
    getInverse <- function() im
    
    ## returns a list of functions within the makeCacheMatrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function takes a List (makeCacheMatrix data)
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x', if previously calculated
    im <- x$getInverse()
    
    ## if data in im is not null (meaning it was cached), return that data
    if(!is.null(im)) {
        message("getting cached data")
        return(im) ## returns from function im data
    }
    
    ## matrix is retrieved from x$get function ($get is part of makeCacheMatrix)
    data <- x$get()
    
    ## pass matrix (in data variable) to solve function to return the inverse matrix
    im <- solve(data, ...)
    
    ## set the inverse value using $setInverse function
    x$setInverse(im)
    
    ## return im value
    im
}
