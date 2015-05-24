## Put comments here that give an overall description of what your
## functions do

## This function defines a list with 4 functions and two data containers
## 1st data container: mtrx - the matrix to be inversed
## 2nd data container: imtrx - the inverse of mtrx
## 1st function: set - stores the matrix to be inversed
## 2nd function: get - returns the original matrix
## 3rd function: setinverse - stores the cached value
## 4th function: getinverse - retrieves the cached value

makeCacheMatrix <- function(mtrx = matrix()) {
  imtrx <- NULL
  set <- function(y) {
    mtrx <<- y
    imtrx <<- NULL
  }
  get <- function() mtrx
  setinverse <- function(mean) imtrx <<- mean
  getinverse <- function() imtrx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function defines the cached version of solve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##attempts to retrieve the inverse from cache
  imtrx <- x$getinverse()
  if(!is.null(imtrx)) {
    ##if found, return the cached data
    message("getting cached data")
    return(imtrx)
  }
  #if not found, retrieve the original matrix
  data <- x$get()
  #find its inverse using solve
  imtrx <- solve(data)
  #set the new cache value to be retrieved on the next invocation
  x$setinverse(imtrx)
  #return the inverse
  imtrx
}


##unit test

## 1. define test data

mtx=rbind(c(1, 5), c(5, 1))

## 2. initialize the cache matrix with the data above

cacheMatrix <- makeCacheMatrix(mtx)

## 3. on the first call, solve is invoked and the "getting cached data" is not displayed

imtx <- cacheSolve(cacheMatrix)
imtx

## 3.1 verify the result is correct
prd <- mtx %*% imtx
prd

## 4. on the second call, data is returned from the cache and the message "getting cached data" is displayed

imtx <- cacheSolve(cacheMatrix)
imtx

## 4.1 verify the result is correct
prd <- mtx %*% imtx
prd
