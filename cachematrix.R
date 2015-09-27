## R Programming - Programming Assignment 2

## makeCacheMatrix function creates a "matrix object" that can store matrix and cache its inverse

makeCacheMatrix <- function(origMtx = matrix()) {
    ## Make a special "matrix object" with two data fields (origMtx and invMtx)
    ## and four methods (set, get, setInv, and getInv)
    invMtx <- NULL
    
    set <- function(x) {
        origMtx <<- x
        invMtx <<- NULL
    }
    
    get <- function() origMtx
    
    setInv <- function(xinv) invMtx <<- xinv
    
    getInv <- function() invMtx
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve function computes the inverse of the matrix stored in the object returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## 'X' is an "object" created by makeCacheMatrix function
    inv <- x$getInv()
    if (!is.null(inv)) {
        #message("Getting cached data...")
        return(inv)
    }
    #message("Inverting and caching...")
    mtx <- x$get()
    ## We assume that mtx is always invertible
    inv <- solve(mtx)
    x$setInv(inv)
    inv
}
