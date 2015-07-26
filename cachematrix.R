
# Function to compute the inverse of a matrix with caching.
# Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  ##set the matrix
  ##get the matrix
  ##set the inverse
  ##get the inverse
  
  t = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object 
    x <<- y
    t <<- NULL
  }
  get = function() x
  setinv = function(inverse) t <<- inverse 
  getinv = function() t
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

#########################################################################

# Calculates the inverse of an assumed square matrix, or retrieves
# a previously calculated inverse from cache, given a list argument
# from the function makeCacheMatrix().


cacheSolve <- function(x, ...) {

  t = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(t)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(t)
  }
  
  mat.data = x$get()
  t = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(t)
  
  return(t)
}
  
  
##############################################################################  
  
##### Test out the functions
test = function(mat){

  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}





## matrix of 500 rows and 500 columns filled with normal random numbers.

set.seed(252000)
r = rnorm(250000)
mat1 = matrix(r, nrow=500, ncol=500)
test(mat1)

##Time difference of 1.112064 secs
##getting cached data
##Time difference of 0.001000166 secs


  
