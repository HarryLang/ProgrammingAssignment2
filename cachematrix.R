## special "matrix" object (really just a list) for caching the solve() operation

## return a list for cached inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(theInverse) inv <<- theInverse
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## find matrix inverse, using cached value if available
cacheSolve <- function(x, ...) {
    if(!is.null(inv)) {
        print("getting cached inverse")
        return(inv)
    }
    M <- x$get()
    inv <- solve(M)
    x$setInv(inv)
    inv
}
