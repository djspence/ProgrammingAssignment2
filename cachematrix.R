## Programming Assignment #2
## D. J. Spence  09/23/2015
## R Programming Coursera Online Course
## Two functions that facilitate matrix inverse caching
## Functions are makeCacheMatrix and cacheSolve
## Solution is modeled after sample functions makeVector and cachemean,
##             written by the course instructors (Peng et al)

## makeCacheMatrix 
##   - Creates a structure to keep track of a matrix and its inverse
##   - The structure is a list with the following items in it
##   - 1) set: a function to set the value of the matrix (sets inverse to NULL)
##   - 2) get: a function to retrieve the value of the matrix
##   - 3) setInverse: a function to set the value of the inverse
##   - 4) getInverse: a function to retrieve the value of the inverse
##        setInverse and getInverse shouldn't be used directly;
##        they are only for use within the makeCacheMatrix function.
##        (If I knew how to make them protected/private, I would)

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(matInv) inv <<- matInv
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
##    - This function keeps a cache of a matrix inverse
##    - Works with member functions created by makeCacheMatrix
##    - If inverse has already been computed, it is returned from cache
##    - Otherwise, it is computed, cached, and returned

cacheSolve <- function(x, ...) 
{
    inv <- x$getinverse()
    if(!is.null(inv)) 
    {
        message("reading inverse from cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
