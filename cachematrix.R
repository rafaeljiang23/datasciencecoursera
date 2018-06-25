## Put comments here that give an overall description of what your
## functions do
## These functions are used to cache the inverse of an invertible matrix

## Write a short comment describing this function
## This function create a "matrix" object that can caches its inverse
makeCacheMatrix <- function(x = matrix()){
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInv <- function(inv) i <<- inv
    getInv <- function() i
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolveshould retrieve the
## inverse from the cache.

cacheSolve <- function(x){
    i <- x$getInv()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data)
    x$setInv(i)
    i
}
