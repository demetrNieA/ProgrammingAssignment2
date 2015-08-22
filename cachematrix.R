## Second programming assesment for R Programming course.
##
## Functions in this module ARE NOT INTENDED FOR PRODUCTION USE.
##
## Functions in this module can be used to create holder for a matrix and
## caching inverse of it. Correct usage:
##
## a <- matrix(...)
## holder <- makeCacheMatrix(a)
## holder1 <- cacheSolve(holder)
## holder1$get() ## Inversed matrix
##
## For the purpose of the task we assume that the matrix supplied is always
## invertible.


## This function creates a special "matrix" object holder that can cache
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    ## Clear cached value if matrix is changed.
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  
  ## returning holder to make matrix immutable.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This is cachable version of solve() function. Matrix should be passed
## by holder created by the makeCacheMatrix() function.
## 
## In case this function called without additional arguments it returns
## (and performs caching) new holder with inverse of the passed matrix.
## Otherwise it pass this arguments to solve() function and returns holder
## of result.
cacheSolve <- function(x, ...) {
  unnamedArguments <- list(...)
  if (length(unnamedArguments) > 0) {
    ## As we have further arguments passed to or from other methods
    ## we can't cache result: solve(x, y) <> solve(x)
    warning("Got several unnamed arguments, result can't be cached")

    result <- solve(x$get(), ...)
    holder <- makeCacheMatrix(result)
  } else {
    ## inverse of a matrix case

    if (is.null(x$getInverse())) {
      ## If we do not have result yet - calculate it.
      inverse <- solve(x$get())
      x$setInverse(inverse)
    }

    ## As solve(solve(x)) == x if x isinvertible we can cache
    ## inverse for a new one without additional calculations.
    holder <- makeCacheMatrix(x$getInverse())
    holder$setInverse(x$get())
  }
  
  holder
}

## Test correctness
cacheSolveTest <- function() {
  
  ##    [,1] [,2] [,3]
  ## [1,]  0    1    0
  ## [2,]  1    1    0
  ## [3,]  1    0    1
  a <- matrix(c(0, 1, 1, 1, 1, 0, 0, 0, 1), 3, 3)

  holder <- makeCacheMatrix(a)
  if (!is.null(holder$getInverse())) {
    warning("Inverse should not be created yet!")
  }
  if (!(all(a == holder$get()))) {
    warning("Incorrect holder!")
  }

  ##    [,1] [,2] [,3]
  ## [1,] -1    1    0
  ## [2,]  1    0    0
  ## [3,]  1   -1    1
  b <- matrix(c(-1, 1, 1, 1, 0, -1, 0, 0, 1), 3, 3)

  holder1 <- cacheSolve(holder)
  if (!(all(b == holder1$get()))) {
    warning("Incorrect inverse matrix")
  }
  holder1 <- cacheSolve(holder)
  if (!(all(a == holder1$getInverse()))) {
    warning("Do not cache inverse for inverse!")
  }

  holder2 <- cacheSolve(holder, b = b)
  if ((all(b == holder2$get()))) {
    warning("We should not have inverse matrix in case
            we pass additional arguments!")
  }
}