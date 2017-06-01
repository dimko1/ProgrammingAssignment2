## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	    ## setting up data for the matrix
	    m <- NULL
	    
	    ## creating setter 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## creating getter function
        get <- function() x
        
        ## setting inverse value
        setinverse <- function(inverse) m <<- inverse
        ##retrieving inverse value
        getinverse <- function() m
        
        ## returning delegates for function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
		## trying to get inverse value
        m <- x$getinverse()
        
        ## it is not null, so let's return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if we are here - is it null. let's calculate and cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
