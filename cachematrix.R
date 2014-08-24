## This script contains two functions, makeCacheMatrix() and cacheSolve(),
## that work together in order to create a "matrix object"
## that holds the inverse of a square (dimension n x n), 
## non singular matrix (matrix determinant is not 0).
##


## makeCacheMatrix() creates the "matrix object" from a square matrix 'x'.
## The function: 
## (1) set the matrix of the "matrix object"
## (2) get the matrix of the "matrix object"
## (3) set the inverse of the matrix onto the "matrix object"
## (4) get the inverse of matrix of the "matrix object"

makeCacheMatrix <- function(x = matrix()) {             ## input 'x' is a square non singular matrix
    invMatrix <- NULL                                   ## the inverse of the matrix 'x' (invMatrix) is set to NULL
                                                        ## when object is created
    
    set <- function(y) {                                ## takes an input matrix
        x <<- y                                         ## saves the input matrix
        invMatrix <<- NULL                              ## resets the inverse of the matrix to NULL
    }
    
    get <- function() x                                 ## returns matrix 'x'
    
    setInvMatrix <- function(solve) invMatrix <<- solve ## computes the matrix inverse and is only
    ## accessed by cacheSolve() when invMatrix is NULL
    ## (first call to cacheSolve())
    
    getInvMatrix <- function() invMatrix                ## return the cached inverse to cacheSolve()
    
    ## list with all the "methods" that are part of the "matrix object" so that they can be accessed externally
    ## by cacheSolve()
    list(set = set, get = get,                          
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## When called cacheSolve() wil see if the inverse of the matrix 'x' 
## associated to the "matrix object" has been cached (not NULL). 
## Depending on the result,
## cacheSolve() either computes the inverse of the matrix 'x' 
## returned by makeCacheMatrix() by using the solve() function 
## and saves it in the "matrix object" or
## return cached inverse from the "matrix object".

cacheSolve <- function(x, ...) {            ## input 'x' is a "matrix object"
    
    invMatrix <- x$getInvMatrix()           ## get the inverse of the matrix 'x'
    
    ## check if the inverse of the matrix 'x' was already cached (not NULL), if so,
    if(!is.null(invMatrix)) {              
        message("getting cached data")      ## tell the user by printing a message to the console,
        return(invMatrix)                   ## return cached inverse and ends the function
    }
    
    ## If the inverse of the matrix 'x' was not yet cached ( is NULL),
    data <- x$get()                         ## get matrix 'x' from "matrix object"
    invMatrix <- solve(data, ...)           ## compute the inverse of matrix 'x' using the solve() function
    x$setInvMatrix(invMatrix)               ## store the calculated inverse in "matrix object"
    invMatrix                               ## return the inverse of matrix 'x'
}
