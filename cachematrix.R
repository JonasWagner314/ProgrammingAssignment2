## The first function is similar to the makeVector function of the example.
## First the elements of the matrix are set
## Then we get the elements of the matrix
## Then we set the elemnts for the inverse matrix
## Finally we get the elements of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
                    m <- NULL             ##set inverse as NULL
                    set <- function(y){   ##set elements of matrix
                      x <<- y
                      m <<- NULL
                    }
                    get <- function() x     ##get matrix
                    setinverse <- function(inverse) m <<- inverse   ##set inverse
                    getinverse <- function() m    ## get inverse matrix
                    list(set = set, get = get,
                         setinverse = setinverse,
                         getinverse = getinverse)
}


## this function is also similar to the example. 
## the function checks whether the inverse matrix has already been calculated.
## If so, it returns the cached data, if not it will calculate the inverse.

cacheSolve <- function(x, ...) {
               m <- x$getinverse()            ##Gets inverse matrix
              if(!is.null(m)) {               ##Check whether inverse has been calculated
              message("getting cached data")
              return(m)                       ##Return cached data
             }
 
               matrix_to_invert <- x$get()   ##Otherwise gets matrix
               m <- solve(matrix_to_invert, ...)  ##solves matrix
               x$setinverse(m)
               m                      
}
