## 1.The function written below is a pair of functions that cache and compute the inverse of a matrix.

## 2. This function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(mx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special "matrix" returned by "makeCacheMatrix" above. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Then the "cachesolve" should retrieve the inverse from the cache.

##Here is the cache function

cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)
}
