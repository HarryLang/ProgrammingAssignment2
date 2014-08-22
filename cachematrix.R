## special "matrix" object (really just a list) for caching the solve() operation

## return a list for cached inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { ## changes the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x ## returns the matrix
    setInv <- function(theInverse) inv <<- theInverse ## stores the inverse
    getInv <- function() inv ## returns the cached inverse
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## find matrix inverse, using cached value if available
cacheSolve <- function(x, ...) {
    if(!is.null(inv)) { ## if inverse has already been computed
        print("getting cached inverse")
        return(inv)
    }
    M <- x$get() ## the matrix
    inv <- solve(M)
    x$setInv(inv) ## stores inverse for future use
    inv
}
