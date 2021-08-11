## Put comments here that give an overall description of what your
## functions do

## Esta función crea un objeto del tipo matriz que puede almancenar
## en cache su inversa.

makeCacheMatrix <- function(x = matrix()) {
        
  p<-NULL
  set <- function(matrix){
    m<<-matrix
    p<<-NULL
  }
  get<-function(){
    m
  }
  setInverse<-function(){
    p<<-inverse
  }
  getInverse<-function(){
    p
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## La función calcula la inversa de la matriz creado con la función anterior.
## Sin embargo, lo que hace primero es verificar si la inversa ya ha sido
## calculada. De ser así, no realiza el cálculo y obtiene la inversa del cache

cacheSolve <- function(x, ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cache data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)%*%data
  x$setInverse(m)
  m
        
}
