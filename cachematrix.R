# The functions create a special matrix that can keep
# cache of the matrices data and its inverse

# makeCacheMatrix creates a matrix which is a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inverse
    getinverse <- function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function calculates the inverse of the matrix created with the function above. 
# It first checks if the inverse has already been calculated. If so, it gets the inverse from 
# the cache and skips the computation. otherwise, it computes the inverse, sets the value in
# the cache via the setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data.")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
