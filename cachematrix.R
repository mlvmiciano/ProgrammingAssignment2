makeCacheMatrix <- function(a = matrix()) {
  q <- NULL
  set<- function(b){
  a<<- b
  q<<- NULL
}
get<-function() a
setmean <- function(mean) q <<- mean
getmean <- function() q
list (set = set, get = get, 
      setmean = setmean, 
      getmean = getmean)
}

cachesolve <- function(a, ...) {
  q <- a$getmean()
  if(!is.null(q)){
    message("getting cached data")
    return(q)
  }
  Data <- a$get()
  q <- solve(Data, ...)
  a$setmean(q)
  q
}

