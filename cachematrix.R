## makeCacheMatrix takes a matrix as an argument and returns an R object
## that stores the argument matrix and its inverse. 
## cacheSolve takes an input argument of the type makeCacheMatrix(), returns
## the cached value of m if it has already been computed or computes the inverse
## of the matrix x, returns the value and stores it in m if it hasn't


## creates R object containing the matrix x and its inverse m

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


## If the inverse of x has already been computed, retrieves the value from m 
## otherwise, compute the inverse, return its value and store it in m. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data, hopefully")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
