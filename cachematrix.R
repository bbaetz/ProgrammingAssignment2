## These functions are used to create a matrix that
## will have its inverse calculated on demand, and cached
## This allows for efficient use of the inverse
## if it needs to be calcualted multiple times

## Create a matrix object that is able to have its inverse cached

makeCacheMatrix <- function(x = matrix()) {
    # Initialise the inverse value
    inv <- NULL
    # Set the matrix, and clear the cached inverse (since its no longer valid)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Return the matrix
    get <- function() x
    # Set/get the inverse
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    # And finally, return all of the above as a list
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Return the inverse of the given matrix
## This is only calculated the first time;
## subsequent requests return a cached result

cacheSolve <- function(x, ...) {
    # Try to get the inverse
    inv <- x$getinverse()
    # If its cached, return that value
    if(!is.null(inv)) {
        #Uncomment when debugging
        #message("Using cached value")
        return (inv)
    }
    # Otherwise:
    # 1. get the underlying matrix
    data <- x$get()
    # 2. calculate the inverse
    inv <- solve(data, ...)
    # 3. store it in the cache object
    x$setinverse(inv)
    # 4. and return the result
    inv
}
