## Creates a special cache of a matrix object and later inverts the matrix


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) invr <<- inverse
        getInverse <- function() invr
        list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invr <- x$getInverse()
        if (!is.null(invr)) {
                message("Retrieving cached data...")
                return(invr)
        }
        matr <- x$get()
        invr <- solve(matr, ...)
        x$setInverse(invr)
        invr
}