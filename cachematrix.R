## The first function, makeCacheMatrix creates a special "matrix", 
##which is really a list containing a function to
##set the matrix
##get the matrix
##set the inverse matrix
##get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(solve) m <<- solve
        getinvmatrix <- function() m
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}        
}


## The following function calculates the mean of the special "matrix" 
##created with the above function. However, it first checks to see if the 
##inverse matrix has already been calculated. If so, it gets the mean from 
##the cache and skips the computation. Otherwise, it calculates the inverse
##of the data and sets the inverse matrix in the cache via the setinvmatrix
##function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmatrix(m)
        m
}