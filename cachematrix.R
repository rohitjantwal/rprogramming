## Two functions makeCacheMatrix and cacheSolve which creates
## a matrix that caches its inverse and computes inverse of 
## the matrix returned by makeCachematrix respectively.
## Also checks if inverse has already been calculated, if yes
## then cacheSolve retrieves the inverse from the cache.

## Function to create matrix and cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	i <- NULL

    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    get <- function() {
    	m
    }

    setInverse <- function(inverse) {
        i <<- inverse
    }

    getInverse <- function() {
        i
    }

    ## list of methods returned
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Function to compute the matrix returned by makeCacheMatrix. If inverse has
## been calculated then this retrieves the inverse from cache.
cacheSolve <- function(x, ...) {

    m <- x$getInverse()

    if( !is.null(m) ) {
            message("Cache matrix retrieved")
            return(m)
    }

    data <- x$get()

    m <- solve(data) %*% data

    x$setInverse(m)

    m
}