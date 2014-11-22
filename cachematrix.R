## These function are used for caching an inverse matrix in order to avoid
## computing the same inverse matrix several times.
## 
## Example:
## # create the cached matrix for a
## a <- makeCacheMatrix(matrix(c(1, 0, 0, 1), 2, 2))
## for(i in 1:100) {
##   invA <- cacheSolve(a) # the inverse of a is computed only once
##   # do something with the inverse of a
## }

## This function creates a cached matrix.
## It allows computing the inverse matrix only once.
##
## Example:
## a <- makeCacheMatrix(matrix(c(1, 0, 0, 1), 2, 2))
## a$get() # returns stored matrix
## b$set(matrix(c(2, 0, 0, 2), 2, 2)) # changes stored matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(value) {
                x <<- value
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function returns the inverse of a cached matrix.
##
## Example:
## a <- makeCacheMatrix(matrix(c(1, 0, 0, 1), 2, 2))
## inv <- cacheSolve(a) # computes the inverse of a and saves the result
## inv2 <- cacheSolve(a) # doesn't compute again, just returns
## a$set(matrix(c(2, 0, 0, 2), 2, 2))
## inv3 <- cacheSolve(a) # a matrix has been changed, so compute a new inverse
## inv4 <- cacheSolve(a) # returns the previously computed result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                return(inv)
        }
        inv <- solve(x$get())
        x$setInverse(inv)
        inv
}
