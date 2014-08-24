## D. Feger (Aug 24, 2014)
## The following functions allow the user to create and cache a matrix object
## and subsequently generate and cache, or automatically retrieve, the 
## inverse of the cached matrix object.

## The makeCacheMatrix function allows the user to create an object that caches
## a matrix and provides methods for retrieving the matrix, caching the inverse
## and retrieving the inverse.

makeCacheMatrix <- function(x = matrix()) 
    {
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(y) m <<- solve(y)
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
    }


## The cacheSolve function allows the user to generate and cache the inverse of 
## a previously cached matrix object.  If the inverse has been previously cached,
## the function will retrieve, instead of generating, the cached inverse.

cacheSolve <- function(x, ...) 
    {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(matrix)
    m
    }

