## The below functions create a special type of matrix that
## allows the inverse of the matrix to be cached.
## If the cache is not yet set, then the inverse is calculated
## and then the inverse is cached for the next call to get the
## inverse.



## This function creates a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        ## Set the matrix to be evaluated.
        setMatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## Return the matrix that is being evaluated.
        getMatrix <- function() {
                x
        }
        
        ## Set the inverse to be cached.
        setInverse <- function(i) {
                inverse <<- i
        }
        
        ## Get the cached inverse.
        getInverse <- function() {
                inverse
        }
        
        ## Return a list of functions to get and set
        ## the matrix, and to get and set the cache for
        ## of the inverse.
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix, 
             setInverse = setInverse, 
             getInverse = getInverse)
}



## This function calculates the inverse of the matrix
## that is created with the makeCacheMatrix.
## The function returns the cached inverse if it has
## been calculated, otherwise it will caculate the 
## the inverse and cache it for the next call to 
## cacheSolve.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getInverse()
        
        ## Check if the inverse has already been calculated.
        ## If it has then return the result.
        if (!is.null(i)) {
                message("Getting cached data.")
                return(i)
        }
        
        ## The inverse has not been calculated.
        ## Then calculated the inverse and cache it.
        data <- x$getMatrix()
        i <- solve(data)
        x$setInverse(i)
        
        i
}
