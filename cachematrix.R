## Functions to use for creating an inverse-cacheable matrix
## and to calculate the inverse of that matrix

## Wrap a matrix to store its inverse 

makeCacheMatrix <- function(x = matrix()) {
    
    #store the cached version of the matrix
    inverse <- NULL
    
    #set the matrix and delete cached inverse
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    #get the stored matrix
    get <- function() {
        x
    }
    
    #set the inverse og the matrix
    setinverse <- function(inv) {
        inverse <<- inv
    }
    
    #get the inverse of the matrix
    getinverse <- function() {
        inverse
    }
    
    #return public functions
    list (
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
    
}


## Wrap the solve function in order to use cached version of matrix 
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    
    #get the inverse from argument
    inv <- x$getinverse()
    
    if (!is.null(inv)) {
        #if present, do nothing
        message("getting cached inverse")
    } else {
        #else, calculate the inverse and store it
        message("calculating matrix inverse")
        inv = solve(x$get(), ...)
        x$setinverse(inv)
    }
    
    #return the inverse
    inv
}
