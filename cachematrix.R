
## The makeCacheMatrix function ultimately returns a list of the inverse.

makeCacheMatrix <- function(x = matrix()) {   #Default of argument x is an empty matrix.
        m <- NULL                             #Creates inv, an empty object to be used later.
        set <- function(y) {                  # function with argument y
                x <<- y                       # y is assigned to x, the matrix in parent eniviroment of makeCacheMatrix.
                m <<- NULL                    # NULL is assigned to inv, also in parent enviroment.
        }
        get <- function() x                   # x is not defined within function get(), therefore R retrive it from parent enivorment.
        setsolve <- function(solve)           # setter function for the inverse.
            m <<- solve                       # inv is in parent enivorment.
        getsolve <- function() m              # R retrive the value of inv from parent enivorment.
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)             # Finally, a list is returned.
}

## cacheSolve computes the inverse of the list that is returned from makeCacheMAtrix.
## If the inverse has been computed already, then this function will get the inverse from the cache.
## This speed things up.

cacheSolve <- function(x, ...) {              # argument x refers to the list makeCacheMatrix returns.
        m <- x$getsolve()                     # Return a matrix that is the inverse of 'x'
        if(!is.null(m)) {                     # If the result is NULL..
              message("getting cached data")
              return(m)                       # ...then return the value inv to parent enviroment.
        }
        data <- x$get()                       # If !is.null is FALSE, the function get x.
        m <- solve(data, ...)                 # And calucalte the inverse.
        x$setsolve(m)
        m                                     # Returns the inverse.
}
