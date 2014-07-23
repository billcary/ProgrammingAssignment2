## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object
## that can cache its inverse. It provides methods
## to set and get the original matrix, as well as
## methods to set and get the inverse of the matrix.

makeCacheMatrix <- function(mtrx = matrix()) {
        ## set initial values and initialize caching of both
        ## the original matrix and the inverse.
        ## Establish the cached value of the original matrix
        inv <- NULL
        set <- function(y) {
                mtrx <<- y
                inv <<- NULL
        }
        
        get <- function() mtrx  ## provide a get method that returns orig. matrix
        setinv <- function(solve) inv <<- solve  ## compute inverse using solve()
        getinv <- function() inv  ## provide method to return the inverse
        
        ## expose methods created above for use by calling functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        ## if cached value for inv exists,
        ## then return the value from the cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if the cached value of the inverse does not
        ## exist, then get the original matrix from the cache
        ## and then calculate its inverse using solve()
        data <- x$get()
        inv <- solve(data, ...)
        
        x$setinv(inv)  ## set the value of the inverse in the cache
        inv  ## return the inverse
}
