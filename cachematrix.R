# There is obviously too much commentary in this, far and above what I would
# reasonably add to any program, but this assignment was challenging for me and
# I needed to reason it out to (hopefully) understand it.

# cacheSolve takes a matrix created by makeCacheMatrix and checks whether the 
# inverse matrix has been solved. If the inverse matrix is solved, the user can
# get the matrix or inverse matrix cached in the parent environment.  If the
# matrix has not been solved, cacheSolve solves the matrix, setting (or
# defining) the matrix and inverse matrix and caching those in the parent 
# environment

# makeCacheMatrix creates a list of functions that get and set the matrix and
# inverse matrix so that constructions like x$get or x$set can get or set the
# matrix or inverse matrix
makeCacheMatrix <- function(x = matrix()) {
      invMatrix <- NULL       # initializes an empty variable invMatrix that 
                              # will be used to store the inverse matrix 
       
      # defines a set function that will define the matrix when x$get() is
      # called at the console. The variable y stands in for the matrix that 
      # one wishes to define (or "set"). y is passed into the parent
      # environment as x which then sets a new matrix for evaluation, while the
      # invMatrix is nullified in the parent environment
      set <- function(y) {
            x <<- y     # y is a dummy variable that changes the value of x in 
                        # the parent environment (to y)
            invMatrix <<- NULL  # sets the inverse matrix in parent
                                # environment (to NULL)
      }
      
      # defines a get function that returns x.  If one calls x$get(), where x is
      # a previously defined (or "set") matrix, the matrix defined by the set() 
      # function (where dummy variable "y" was passed to the parent 
      # environment's x) is returned.
      
      get <- function() {
            x
      }
     
      # defines the setInverse function.  When cacheSolve calls 
      # x$setInverse(inverse), store inverse as the invMatrix variable in 
      # the parent environment, caching the inverse matrix
      
      setInverse <- function(inverse) {
            invMatrix <<- inverse
      }
      
      # defines the getInverse function.  When cacheSolve(x) calls
      # x$getInverse() search the parent environment for the cached invMatrix
      # variable
      
      getInverse <- function() {
            invMatrix
      }
      
      # creates a list of functions that are the ultimate output of the
      # makeCacheMatrix function used by cacheSolve
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


# cacheSolve uses the list of functions from makeCacheMatrix to check whether
# a matrix x is cached in the parent environment and to get whatever infromation
# about the matrix is cached, thereby speeding up the computation and decreasing
# memory usage
cacheSolve <- function(x, ...) {
            invMatrix <- x$getInverse() # Get the inverse matrix value from the 
                                        # parent environment
            # if the inverse matrix is not a null entity (i.e., the inverse 
            # matrix is cached in the parent environment) state that the function
            # is getting the cached inverse matrix and return the value, exiting
            # the function
            if(!is.null(invMatrix)) {
                  message("getting cached data")
                  return(invMatrix)
            }
            
            # in cases where there is no cached matrix
            data <- x$get() # set data equal to the value of the cached matrix 
            
            invMatrix<- solve(data, ...) # solve for the inverse matrix 
            
            x$setInverse(invMatrix) # set the value of the inverse matrix 
            
            invMatrix # print the inverse matrix
}
