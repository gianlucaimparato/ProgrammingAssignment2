## This set of functions can be used to cache the inverse of a given matrix.

## The makeCacheMatrix function accepts a matrix as an argument and returns a list of
## functions to
## - set or update the matrix whose inverse is to be cached (setMatrix)
## - retrieve the saved matrix (getMatrix)
## - cache the inverse of the matrix (setInverse)
## - retrieve the cached inverse matrix (getInverse)

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL          ##initial assignment for inverse_matrix
        
        setMatrix <- function(y){
                x <<- y
                inverse_matrix <<- NULL
                }
        
        getMatrix <- function() x
        
        setInverse <- function(inverse) inverse_matrix <<- inverse
        
        getInverse <- function() inverse_matrix
        
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function accepts the list of functions provided by 
## makeCacheMatrix as an argument and:
## - retrieves the inverse of the saved matrix with the getInverse function.
## - if there is a cached inverse, returns the cached data.
## - if not, the solve function is used to calculate the inverse of the saved
##    matrix which is retrieved by the getMatrix function.
## - the setInverse function is then called to update the cached inverse.
## - returns the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()       ##retrieve cached data
        
        if(!is.null(inverse)){ 
                message("getting cached inverse matrix")
                return(inverse)         ##if cached data are available, they are
                                        ##returned
        }
        
        data <- x$getMatrix()           
        inverse <- solve(data, ...)     ##else, the inverse matrix is calculated
        x$setInverse(inverse)           ##cached data are updated
        inverse                         ##the inverse matrix is returned
        
}
