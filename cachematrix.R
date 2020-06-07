## functions that cache the inverse of a matrix

## creates special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

    ## initialize inverse property
    i <- NULL

    ## method to set matrix
    set <- function(y) {
            x <<- y
            i <<- NULL
    }

    ## method get matrix
    get <- function() {
    	## return matrix
    	x
    }

    ## method to set the inverse of matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## method to get the inverse of matrix
    getInverse <- function() {
    ## return inverse property
        i
    }

    ## return a list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## compute inverse of special matrix returned by "makeCacheMatrix" above.
## if the inverse has already been calculated (and matrix has not
## changed), then "cachesolve" should retrieve the inverse from cache.

cacheSolve <- function(x, ...) {

    ## return a matrix that is inverse of 'x'
    i <- x$getInverse()

    ## just return inverse if its already set
    if( !is.null(i) ) {
            message("getting cached data")
            return(i)
    }

    ## get matrix from our object
    data <- x$get()

    ## calculate inverse using matrix multiplication
    i <- solve(data) %*% data

    ## set inverse to object
    x$setInverse(i)

    ## return matrix
    i
}
