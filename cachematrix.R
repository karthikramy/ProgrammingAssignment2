## Program consists of two funtion makeCacheMartix and CacheSolve
## makeCacheMatrix is to cache an inverse of an matrix
##CacheSolve will compute the inverse of the matrix if its not already in cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ##set the inverse of the matrix in cache
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' from cache or after 
        ##computing the inverse of the matrix
        
        ##check if its already in cache if yes then returns the value from cache
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##if the value is not in cache then inverse the matrix and set the cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        
}
