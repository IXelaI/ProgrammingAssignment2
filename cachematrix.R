# The following functions are written in order to solve a potentially time consuming 
# and resource intensive problem, when trying to find the inverse of a matrix. It may
# be beneficial to cache the inverse of a matrix during the computation. This way
# the cached info can be accessed and used at a later time.

# makeCacheMatrix is a function that creates a list which contains functions
# to set and get the values of the vector and to set and get the values of 
# the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    # create an empty/null variable
    i <- NULL
    # define 'set' function which takes argument 'y'
    set <- function(y) {
        # assign y to x
        x <<- y
        # reset i to NULL
        i <<- NULL
    }
    # define 'get' function which returns the value 'x'
    get <- function() x
    # define 'set.inverse' function which takes argument 'solve'
    # assigns the value of 'solve' to 'i'
    set.inverse <- function(solve) i <<- solve
    # define 'get.inverse' function, which returns value 'i'
    get.inverse <- function() i
    # a list of the functions 'set', 'get', 'set.inverse', 'get.inverse'
    # that gets returned
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}

# cacheSolve is a function that calculates the inverse of the special "vector" created
# by makeCacheMatrix function. It does that by first checking to see if the inverse of
# the matrix has already been calculated or not. If it has, it uses the cached value, 
# otherrwise it calculates the inverse of the matrix.
cacheSolve <- function(x, ...) {
    # assign the inverse of the matrix from makeCacheMatrix to i
    i <- x$get.inverse()
    # if i is not empty/null then get its cached content
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # otherwise assign to data the value returned by get()
    data <- x$get()
    # solve the inverse of the matrix and assign its value to i
    i <- solve(data, ...)
    # set the inverse of matrix to 'i'
    x$set.inverse(i)
    # return the value of i which is the inverse of the matrix
    i
}