## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the inverse object
        m <- NULL
        
        ## Set the matrix value
        set <- function( y ) {
                x <<- y
                m <<- NULL
        }
        
        ## Get the matrix value
        get <- function() x
        
        ## Set the inverse of the matrix
        setInverse <- function(inverse) m <<- inverse        
        
        ## Get the inverse of the matrix
        getInverse <- function() m
        
        ## Return the methods' list
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        
        ## Return inverse of 'x'
        m <- x$getInverse()
        
        ## If the inverse is calucalated and cashed before, return it
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## If inverse is not calculated before, get the matrix
        data <- x$get()
        
        ## Calculate the inverse of the matrix using solve() method
        m <- solve(data)
        
        ## Set the inverse value
        x$setInverse(m)
        
        ## Return the inversed matrix
        m
}
