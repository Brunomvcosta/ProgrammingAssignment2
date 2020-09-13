##The main goal of the program is to calculate the inverse of a matrix. 
##The first function sets the different functions to be used on the program, set, get, set solve and get solve. 
##The first two get and set matrices. The third and forth get and set values for the inversed matrix
##Once the inversed is calculated, it is stored on cache, to be recalled by the second function if it exists

##The first function gets and sets the value of the matrix and the inverse matrix
makeCacheMatrix <- function(x=matrix(1:4, nrow=2, ncol=2)){
  m <- NULL
  set <- function(y){
    x <<-y
    m <<- NULL
  }
  get <- function()x
  setsolve <- function(solve) m<<-solve
  getsolve <- function()m
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}
##The second function seeks the inversed matrix on Cache. If found, it doesnt calculates the inverse and sets the value of the inversed matrix by the setsolve function
##However, if no inversed matrix is found, it calculates the inverse matrix using the getsolve function
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting inversed matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
