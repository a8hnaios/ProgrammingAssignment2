## Two main function which all one to compute the inverse of a matrix and
## cache it for future retrieval from the cache.
# Function makeCacheMatrix creates a  list of functions
# Function cacheSolve either computes a matrix inverse, caches it
# and returns it or returns a cached inverse

## Function makeCacheMatrix
# Creates a list that is composed of four functions (set, get, setinv, getinv)
# function sets sets a given matrix
# function get returns the matrix
# function setinv sets the inverse of the matrix
# function getienv returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinv <- function(solve) inv <<- solve
   getinv <- function() inv
   list(set = set,
        get = get,
        setinv = setinv,
        getinv = getinv)
}

## Function cacheSolve returns the inverse of a matrix as defined the
## makeCacheMatrix function
## If the inverse has already been computed and cached then the cached
## inverse is returned

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'

   # Attempt to get the inverse of the matrix
   inv <- x$getinv()

   # If the returned inverse is not null (i.e. it's been computed already)
   # then return the cached inverse 
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }

   # If the returned inverse is null then it's not cached so we need
   # to compute it
   data <- x$get()
   inv <- solve(data, ...)

   # Set the computed inverse so we can return it from the cache next time
   x$setinv(inv)

   # Return the inverse
   inv
}
