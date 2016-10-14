## Inverse of Matriz Caching

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
     ## A square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {

     ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()

        # if the inverse has already been calculated

        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        # otherwise, calculates the inverse 
        mat <- x$get()
        inv <- solve(mat, ...)

         # sets the value of the inverse in the cache via the setinv function.
        x$setInverse(inv)
        inv
}
