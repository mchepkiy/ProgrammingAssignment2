## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function takes a matrix as a parameter
## Limitations: assumes that solve(x) will not return an error
## The function returns a list with 4 elements:
## setMatrix, getMatrix, setInverseCache, getInverseCache

makeCacheMatrix <- function(x = matrix()) {
    ## definitions
    i <- NULL
    
    ##functions declaration
    setMatrix <- function(y){
        x <<- y
        i <<- NULL
    }
    getMatrix <- function(){
        x
    }
    setInverseCache <- function(inverse){
        i <<- inverse
    }
    getInverseCache <- function(){
        i
    }
    
    ##returned list
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInverseCache = setInverseCache, 
         getInverseCache = getInverseCache)
}


## Write a short comment describing this function
## The function takes the output from makeCacheMatrix and
## the source matrix as an additional argument
## if the inversed matrix is saved in cache AND the matrix is unchanged
## (which is checked in the second condition) then the cached data is used
## otherwise the solve(x) is used

cacheSolve <- function(x, my_matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverseCache()
    m <- x$getMatrix()
    if(!is.null(i) & identical(my_matrix, m)) {
        message("getting cached data")
        return(i)
    }
    
    i <- solve(m, ...)
    x$setInverseCache(i)
    i
}
