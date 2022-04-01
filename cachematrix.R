#This function aim at computing the inverse of a matrix using caching 
#to reduce the number of computations.


#The makeCacheMatrix function creates a special "vector" that contains a
#function to set the matrix value,to get the matrix value,to set the 
#inverse of a matrix, and to get the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y){
      x <<- y
      invers <-NULL
      }
  get <- function() x
  setinvers <- function(invs)  invers <<-invs
  getinvers <- function() invers
  list(set=set,get=get,setinvers=setinvers,getinvers=getinvers)
  
  }


## Write a short comment describing this function
#The following function is to calculate the inverse of a given matrix.If the
#inverse of the given matrix has already been calculated, it gets the result from
#the cache.If not, it calculates the inverse of this matrix and set this value
#to the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invers <- x$getinvers()
    if(!is.null(invers)){
        message("getting the result from cache. ")
        return(invers)
    }
    d <- x$get()
    invers <- solve(d,...)
    x$setinvers(invers)
    invers
} 
