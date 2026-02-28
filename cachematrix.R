## Funciones para almacenar en cache la inversa de una matriz
## Creado para Programming Assignment 2

## makeCacheMatrix: Crea un objeto matriz especial que puede cachear su inversa
## Retorna una lista con 4 funciones: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL

                # Funcion para establecer la matriz
                set <- function(y) {
                                        x <<- y
                                        inv <<- NULL
                                }

                # Funcion para obtener la matriz
                get <- function() x

                # Funcion para establecer la inversa en el cache
                setinverse <- function(inverse) inv <<- inverse

                # Funcion para obtener la inversa del cache
                getinverse <- function() inv

                # Retornar lista con las 4 funciones
                list(set = set,
                                  get = get,
                                  setinverse = setinverse,
                                  getinverse = getinverse)
        }


## cacheSolve: Calcula la inversa de la matriz especial creada por makeCacheMatrix
## Si la inversa ya fue calculada (y la matriz no cambio), recupera del cache

cacheSolve <- function(x, ...) {
                # Intentar obtener la inversa del cache
                inv <- x$getinverse()

                # Si existe en cache, retornarla
                if(!is.null(inv)) {
                                        message("getting cached data")
                                        return(inv)
                                }

                # Si no existe, calcularla
                data <- x$get()
                inv <- solve(data, ...)

                # Guardar en cache
                x$setinverse(inv)

                # Retornar la inversa
                inv
        }
