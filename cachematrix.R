## Functions to use for creating an inverse-cacheable matrix
## and to calculate the inverse of that matrix

## Wrap a matrix to store its inverse 

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inv) {
        inverse <<- inv
    }
    
    getinverse <- function() {
        inverse
    }
    
    list (
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
    
}


## Wrap the solve function in order to use cached version of matrix 

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    if (!is.null(inv)) {
        message("getting cached inverse")
    } else {
        message("calculating matrix inverse")
        inv = solve(x$get(), ...)
        x$setinverse(inv)
    }
    
    inv
}
