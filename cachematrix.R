# ==========================================================================
# This script consists of two functions that compute and cache the inverse of 
# a matrix. It checks whether the matrix is square and non-singular.
# Use:  
# Generate a matrix m, then do
# x <- makeCacheMatrix(m)
# y <- cacheSolve(x)
#
# Check answer with round(x$get() %*% y) or round(y %*% x$get())
# ==========================================================================


# ========================================================================== 
# makeCacheMatrix: 
#
#   This function creates a special "matrix" object that can cache its inverse.
#
#   Returns: a list.
# ==========================================================================

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        # replaces x by y and m by NULL
        x <<- y
        m <<- NULL
    }
    
    get <- function() x   # get simply retrieves entity x
    setinverse <- function(solve) m <<- solve   # stores result of solve as m
    getinverse <- function() m   # retrieves value m stored by setinverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# ==========================================================================
# cacheSolve:
#
#   This function computes the inverse of the special "matrix" returned 
#   by `makeCacheMatrix` above. If the inverse has already been calculated 
#   (and the matrix has not changed), then `cacheSolve` should retrieve the 
#   inverse from the cache. It checks if the matrix is square and invertible,
#   even though the 1e-15 tolerance may need to be adjusted.
#
#   Returns: a matrix.
# ==========================================================================


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # Check if the inverse has been computed.
    m <- x$getinverse()
    if (!is.null(m)) {
        message("Getting the cached inverse ...")
        return(m)
    }
    
    # Get the matrix.
    data <- x$get()
    
    # Check if the inverse can be computed to avoid the error from the 
    # LAPACK routine.
    if (rcond(data) < 1e-15 || nrow(data) != ncol(data)) {
        message("Matrix is non-square or badly conditioned. Try another example.")
        return(m) # m will be NULL, nothing has been computed.
    }
    
    # Now that the supplied matrix is invertible, calculate the inverse.
    m <- solve(data, ...)
    x$setinverse(m)
    return(m)
    
}
