## The following functions are used to find the inverse of a non-singular square matrix.
## By using this function we are caching the inverse of a matrix rather than compute it repeatedly 

## This function is used to create a cache for the matrix and its inverse in global environment.
## It creates a special "matrix" object that can cache its inverse. This function returns a list of 4 other functions.


makeCacheMatrix <- function(x = matrix()) 
{
        inv<-NULL
        set <- function(y) 
        {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(y) {inv <<- y}
        getinv <- function() {inv}
        list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## This function is used for the actual inverse operation of the matrix. It computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above.If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache. This function returns the inverted matrix

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
