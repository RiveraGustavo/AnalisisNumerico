#------------------------------------------------------------------------------------------
# PRIMER PARCIAL DE ANÁLISIS NUMÉRICO
# NOMBRE: GUSTAVO ANTONIO RIVERA DELGADO
# CARRERA: INGENIERÍA DE SISTEMAS SEXTO SEMESTRE
# PUNTOS 1-B Y 2-A


#------------------------------------------------------------------------------------------
#2. En R:Sean f(x) = ln(x + 2) y g(x) = sin(x) dos funciones de valor real.
#a) Utilice la siguiente formula recursiva con E = 10^8 para el punto de intersecciÓn.:
#------------------------------------------------------------------------------------------




f = function(x){
  log(x + 2)
}
g = function(x){
  sin(x)
}

#intersección
h = function(x){
  return (f(x)-g(x))  
}

# a)  Utilice la siguiente formula recursiva con E = 1e???8 para el punto de intersección.

# El primer y segundo parámetro son aproximaciones de la solución
# Respectivamente corresponden a los valores iniciales de x(n-1) y x(n-2) 

formulaRecursiva = function(x0, x1, tolerancia)
{
  #------------------------------------------
  #   GRAFICA f y g 
  #------------------------------------------
  plot(g, xlim = c(-4,1), ylim = c(-3,3), col = "blue", main = "Grafica de las Funciones f(x) y g(x)", xlab = "x", ylab = "y")
  par(new=TRUE)
  curve(f, type = "l", col="green", axes=FALSE, ylab = "y", xlim = c(-4,1), ylim = c(-3,3))
  points(-1.631, -0.998, col="red")
  abline(h=0, v=0, col="red")
  par(new=FALSE)
  #------------------------------------------
  #   GRAFICA h
  #------------------------------------------
  plot(h, xlim = c(-4,1), ylim = c(-3,3), col = "green", main = "Grafica de la Función h ", xlab = "x", ylab = "y")
  abline(h=0, v=0, col="red")
  
  
  err = abs(x1-x0)
  cont = 0
  x = 0
  iteraciones=c()
  erroresX = c()
  erroresY = c()
  
  
  cat("---------------------------------------------------------------------------\n")
  cat(formatC( c("x_k"," f(x_k)","Error est."), width = -20, format = "f", flag = " "), "\n")
  cat("---------------------------------------------------------------------------\n")
  
  
  while(tolerancia < err)
  {
    
    x = x1 - (h(x1)*(x1-x0))/(h(x1) - h(x0))
    err2 = err
    err = abs(x - x1)
    if(cont != 0)
    {
      erroresX = c(erroresX, err2/abs(x))
      erroresY = c(erroresY, err/abs(x))
    }
    cont  = cont+1
    x0 = x1
    x1 = x
    cat(formatC( c(x ,h(x), err), digits=15, width = -15, format = "f", flag = " "), "\n")
  }
  
  points(x,0, col="red")
  
  plot(h, xlim = c(min(erroresX),max(erroresX)), ylim = c(min(erroresY),max(erroresY)), col = "white", main = "Errores(i) vs Errores(i+1)", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(erroresX, erroresY, type = "l")
  
  erroresX = c(erroresX,erroresY[cont-1])
  iteraciones = c(1:cont)
  plot(h, xlim = c(0,iteraciones[cont]), ylim = c(min(erroresX),max(erroresX)), col = "white", main = "Iteraciones vs Errores", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, erroresX, type = "l")
  
  cat("\n\nLa intersección se encuentra en el punto x=", x, ", E=",err ," iteraciones = ", cont ,"\n")
}

#Advertencia: Los valores iniciales deben ser cercanos a la solucion para que haya convergencia
formulaRecursiva(-1.8,-1.7,1e-8)

#--------------------------------------------------------------------------------
#1. Sea f(n) la efíciencia del algoritmo, medida como el número minimo de operaciones requeridas para
#resolver el problema
#--------------------------------------------------------------------------------

#b) Implemente en R o Python un algoritmo que le permita sumar los elementos de una matriz cuadrada
#A Imprima varias pruebas, para diferentes valores de n y exprese f(n) en notacion O() con una
#grafica que muestre su orden de convergencia.
#
matriz = function(n)
{
  
  datos = sample(1:20, n*n, replace=T) 
  A = matrix(datos, nrow = n)
  return(A)
  
}

sumarElementos = function()
{
  eje_x = c()
  eje_y = c()
  
  i=1
  tamMatrices = c(2:10)
  
  while (i <=length(tamMatrices))
  {
    suma = 0
    n = tamMatrices[i]
    eje_y[i] = n*n
    eje_x[i] = n
    A = matriz(n)
    j=1
    while(j <= n)
    {
      k = 1
      while (k <= n)
      {
        suma = suma + A[j,k]
        k = k+1
      }
      j = j+1
    }
    
    cat("\n------------La suma para la matriz A: ----------------------------\n" )
    print(A) #Imprimir matriz
    cat("\nes = ", suma)
    i = i+1
  }
  
  plot(eje_x, eje_y, main = "funcion O(n^2)", xlab = "Tamaño", ylab = "Iteraciones", type = "o", ylim = c(0,max(eje_y)), xlim = c(0, max(eje_x)) )
  
}

sumarElementos()
