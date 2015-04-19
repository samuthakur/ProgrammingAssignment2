
# The functions given below create a special "matrix" object and cache its inverse.


#The 'makeCacheMatrix' function creates a list of functions to 
# (1) set the value of the matrix
# (2) get the value of the Matrix
# (3) set the value of the inverse of the matrix
# (4) get the value of the inverse of the matrix
# this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The 'cacheSolve' function camputes the Inverse of the matrix created with the 'makeCacheMatrix' function. 
#First, it checks if the Inverse has already been calculated or not. If Yes, then it gets the inverse from the cache and skips the computation.
#If not then , it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inverse <- x$getInverse()
        if(!is.null(Inverse)) {
                message("getting cached data Inverse")
                return(Inverse)
        }
        data <- x$get()
        Inverse <- solve(data, ...)
        x$setInverse(Inverse)
        Inverse
}

