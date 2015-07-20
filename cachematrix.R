## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: 
#   makes a special "matrix" that can cache its inverse
#   which has getter and setter of the matrix and its inverse
# cacheSolve:
#   computes the inverse of a "matrix" created by makeCacheMatrix.
#   If the cached inverse is available, cacheSolve returns it.
#   If not, cacheSolve computes the inverse, caches it to the "matrix" object,
#   and then returns it.

## Write a short comment describing this function

## makeCacheMatrix
# creates a special "matrix" object (actualy a list) that can cache its inverse.
# args:
#   x : initial value of the matrix
# returns:
#   a list which contains:
#       set : setter of the matrix
#       get : getter of the matrix
#       setinverse : setter of the cached inverse
#       getinverse : getter of the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    cached.inverse <- NULL
    
    set <- function(new.matrix){
        x <<- new.matrix
        cached.inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(new.inverse){
        cached.inverse <<- new.inverse
    }
    getinverse <- function() cached.inverse
    
    # return
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve
# computes the inverse of the "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated, 
# then the cachesolve should retrieve the inverse from the cache.
# args:
#   x: a "matrix" created by makeCacheMatrix above
#   ...: additional args of "solve" function
# returns:
#   inverse of x$get()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    
    # if not already cached
    inv <- solve(x$get(), ...)
    x$setinverse(inv)
    inv
}
