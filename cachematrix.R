## These functions cache the inverse of a matrix

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# set <- resets the matrix x with y
# m <<- NULL resets m, the inverse,  to NULL
# get returns matrix x
# setMatInv stores the inverse in variable m and getMatInv returns it
# list() stores the functions set, get, setMatInv, getMatInv

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatInv <- function(MatInv) m <<- MatInv
        getMatInv <- function() m
        list(set = set, get = get,
             setMatInv = setMatInv,
             getMatInv = getMatInv)

}


# cacheSolve calculates the inverse of the matrix and returns it.
# cacheSolve retrieves m from memory and checks it to see if it is not NULL. If it exists it returns m
# and displays a message.
# Else data gets the matrix and calculates the inverse with solve
# x$setMatInv stores in the variable m
# m is the inverse of x, which is returned

cacheSolve <- function(x, ...) {
        m <- x$getMatInv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatInv(m)
        m  ## Return a matrix that is the inverse of 'x'
}
