## Two functions are created to demonstrate Lexical
## scoping and caching. The user inputs a square 
## matrix and finds the inverse of the matrix with
## the solve function. The inverse is saved
## and the saved/cahced inverse is returned if the 
## matrix needs to have the inverse calculated again.

## The makeCachceMatrix function, creates 
## a list containing a function to
## set the value of the matrix,
## get the value of the matrix
## setmatrix the matrix inverse
## getmatrix the matrix inverse


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


## The cacheSolve function calculates the inverse 
## of the matrix created with the makeCacheMatrix 
## function. The function uses Lexical scoping rules   
## and caches the inverse on the first run.
## On subsequent runs, it gets the inverse from  
## the cache.

cacheSolve <- function(x, ...) {
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
