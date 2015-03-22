# These two functions allow a user to create a special matrix object using
# (makeCacheMatrix) from a matrix input and the cacheSolve function 
# compute the inverse of this special matrix.
# Once the inverse has been computed its value is assigned to the special
# matrix object called Inv. This symbol can be called from memory (cache) by the
# cacheSolve function. If this symbol has a Null value the CasheSolve function
# calculates the inverse of the matrix and also assigns this value to the
# object created from the makeCacheMatrix function. 
# If the inverse has previously been calculated it is fetched from memory and 
# the function doesn’t have to re-compute the value. This is an example of 
# lexical scoping. 
# 

## The MakeCacheMatrix function creates a specialmatrix object from a matrix input
# set - Allows the matrix values to be changed and resets the Inv to NULL.
# get - Returns the values of the matrix
# setSolve - Stores the output of cacheSolve solve(data) to the symbol Inv
# getSolve - Returns the value from the object Inv

makeCacheMatrix <- function(x = matrix()) {
      Inv <- NULL
      set <- function(y) {
            x <<- y
            Inv <<- NULL
      }
      get <- function() x
      setSolve <- function(Solve) Inv <<- Solve
      getSolve <- function() Inv
      invisible(list(set = set,get = get,setSolve = setSolve,getSolve = getSolve))
}

## The cacheSolve function computes the matrix inverse
# The function first call the symbol Inv from the specialmatrix object
# If the value of Inv is not Null the value is returned from the specialmatrix object
# If the value of Inv is Null the inverse is computed and the value is sent to 
# the specialmatrix object.
# The Value of the Inverse matrix is returned. A message is provide if it already exists.

cacheSolve <- function(x, ...) {
      Inv <- x$getSolve()
      if(!is.null(Inv)) {
            message("getting cached data.")
            return(Inv)
      }
      data <- x$get()
      Inv <- solve(data)
      x$setSolve(Inv)
      Inv      ## Return a matrix that is the inverse of 'x'
}

# example

Matrix.eg <- matrix(Values <- sample(1:200,(255)^2, replace = TRUE),nrow = length(Values)^.5,ncol = length(Values)^.5)
Special.Matrix <- makeCacheMatrix(Matrix.eg)
head(cacheSolve(Special.Matrix))
#Re-run with message
head(cacheSolve(Special.Matrix))
