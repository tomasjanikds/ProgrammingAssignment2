# The computation of the inverse matrix might be very costly
# in some of the instances. To speed up this process, the below
# functions could be used to cache the inverse of the matrix
# and then use this information from the cache rather than
# computing the inverse again.

# makeCacheMatrix crates a list consisting of functions which:
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse matrix
# - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL # create empty value
    
    set <- function(y) {
        x <<- y # set the original matrix
        inv <<- NULL 
    }
    get <- function() x # get the original matrix
    
    setInv <- function(inverse) inv <<- inverse # set the inverse matrix
    getInv <- function() inv # get the inverse matrix
    
    list(set = set, # create a list of functions
         get = get,
         setInv = setInv,
         getInv = getInv)

}

# cacheSolve returns the inverse matrix. Firstly, it checks if
# the inverse has already been computed. If that is the case, it gets 
# the result from the cache. If not, it computes the inverse, 
# sets the value into the cache and return the inverse.

cacheSolve <- function(x, ...) {

    inv <- x$getInv() # get the inverse matrix from the cache
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv) # if the cache is not empty, return the inverse
    }
    
    mat <- x$get() # get the original matrix
    inv <- solve(mat, ...) # compute the inverse matrix
    x$setInv(inv) # store the inverse matrix into the cache
    inv # return inverse matrix (computed)
}
