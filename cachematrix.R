## This project will compute the inverse of a matrix only after checking
## to see if the inverse was already computed. If it was, then it will
## simple return the cached value.

## Function to set and get the value and inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the above function, 
## first checking to see if the value has already been calculated. 
## If so, it returns the cached value rather than recomputing.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Test
## x <- makeCacheMatrix()
## x$set(matrix(1:4,2,2))
## cacheSolve(x)

