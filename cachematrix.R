# Caching the Inverse of a Matrix...
#write a pair of functions that cache the inverse of a Matix


# Creating a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(mm = matrix()) {
        i <- NULL
        setM <- function(x) { # set values of the matrix
                mm <<- x
                i <<- NULL
        }
        getM <- function() mm #Creating the functions
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(setM = setM, getM = getM, # Lists values of the matrix
             setinverse = setinverse,
             getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already 
#been calculated (and the matrix has not changed), then the 
#cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mm = matrix(), ...) {
        i <- mm$getinverse()  # Create a variable of whatever the inverse was initially
        if(!is.null(i)) { # If mm$setinverse() was run then mm$getinverse would not be 
                          # NULL and it would return the cached value
                message("getting cached data")
                return(i)
        }
        dat <- mm$getM() # Retrieve data 
        i <- solve(dat, ...) # Calculate
        mm$setinverse(i) # Set it into function 
        i
}
