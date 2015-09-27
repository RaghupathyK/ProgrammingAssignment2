#       Matrix inversion is usually a costly computation and there may be some 
#       benefit to caching the inverse of a matrix rather than compute it repeatedly

#       makeCacheMatrix function creates a special "matrix" for a given input 
#       matrix "x", that can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        # set function sets the value  for  the initial matrix  and resets the 
        # inverse value set in the cache
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # gets the original input matrix for the inverse matrix
        get <- function() x
        # sets the inverse matrix value
        setinv <- function(inv) i <<- inv
        # gets the inverse matrix  stored  in the  cache.
        getinv <- function() i
        # return the special "matrix" which contains the essential functions to
        # set and get the values
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#       cacheSolve function computes the inverse of the special "matrix" returned by 
#       makeCacheMatrix above. If the inverse has already been calculated (and the 
#       matrix has not changed), then the cachesolve should retrieve the inverse from 
#       the cache.
cacheSolve <- function(x, ...) {
        # Get the inverse from Cache
        i <- x$getinv()
        # Check if the inverse is available in cache
        if(is.null(i)) {
                # Oops! The inverse is not in the cache, let us solve it and put
                # it in the  cache for  next inverse of the same matrix
                data <- x$get()
                i <- solve(data, ...)
                x$setinv(i)
                return(i)
        }
        else {
                # Found it in the cache !!! save the trouble  and return the 
                # cached value
                message("Cached Data:")
                return(i)
        }
}