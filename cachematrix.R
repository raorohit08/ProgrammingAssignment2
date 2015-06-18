###############################################################################
# Function Name : makeCacheMatrix
# Description   : This function creates a special "matrix" object that can 
#			cache its inverse.
#Arguments	    : x, a matrix 
#Created By	    : Rohit Yadav
#Creation Date  : 18 Jun 2015
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setCache <- function(solve) m <<- solve
        getCache <- function() m
        list(set = set, get = get,
             setCache  = setCache ,
             getCache = getCache)
}

###############################################################################
# Function Name : cacheSolve
# Description   : This function computes the inverse of the special "matrix" 
#			returned by makeCacheMatrix above. If the inverse has already
#			been calculated (and the matrix has not changed), then the 
#			cachesolve should retrieve the inverse from the cache.
#Arguments	    : x, a matrix 
#Created By	    : Rohit Yadav
#Creation Date  : 18 Jun 2015
###############################################################################
cacheSolve <- function(x, ...) {
        m <- x$getCache()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setCache(m)
        m
}
