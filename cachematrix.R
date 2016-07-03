## The following functions work together to calculate the inverse of a supplied matrix and
## cache the value so that it can be quickly looked up without unnecessary calculations

## Example: 
## m <- matrix(1:4,2,2)
## M <- makeCacheMatrix(m)
## n <- cacheSolve(M)

## MR 07/03/2016

## The following function creates a special "matrix", a list containing 4 functions that:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

        # Set default inverse value as NULL
        inv <- NULL

        # Set the value of the given matrix as 'x' and mean as 'NULL'
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        # Get the value of the matrix x
        get <- function() x

        # Set the value of the given inverse to inv
        setinv <- function(inverse) inv <<- inverse

        # Get the value of the stored inverse
        getinv <- function() inv

        # Return a list with the 4 functions
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}


## The following function checks to see if an inverse has already been calculated
## If so, it returns that value. 
## If not, it calculates the inverse of the special matrix sets it in the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # Use getinv to grab the stored value
        inv <- x$getinv()
        
        # Check if inv has a cached value already
        # If so, grab it
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }

        # Else, get the value of the matrix and calculate its inverse
        data <- x$get()
        inv <- solve(data, ...)
        
        # Then set that as the inverse and return it
        x$setinv(inv)
        inv
}
