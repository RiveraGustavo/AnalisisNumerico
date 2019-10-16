  require(bezier)
  require(gridBezier)
  require(pracma)
  
  #-----------
  # BEZIER SPLINES 
  #-----------
  t <- seq(0, 4, length=100)
  p <- matrix(c(0,7,1,
                0.33,6.92,1,
                0.63, 6.62,1,
                0.7,6.2,1,#1
                2,5.656,1,
                3,5.196,1,
                4.2100,4.2100,1,#2 
                5.196,3,1,
                5.656,2,1,
                6.2,0.7,1,#3
                6.62,0.63,1,
                6.92,0.33,1,
                7,0,1), nrow=13, ncol=3, byrow=TRUE)
  
  bezier_points <- bezier(t=t, p=p, deg =3)
  plot(bezier_points)
  
  
 
  w1 = smooth.spline(p[1:4,1],p[1:4,2])
  w2 = smooth.spline(p[4:10,1],p[4:10,2])
  w3 = smooth.spline(p[10:13,1],p[10:13,2])
  lines(w1,col = "red",lwd = 2)
  lines(w2,col = "red",lwd = 2)
  lines(w3,col = "red",lwd = 2)
  a = matrix(data = p[1:4][1:2],nrow = 4,ncol = 2)
  a
  #-----------
  # PUNTO
  #-----------
  X1= bezier_points[,1]
  Y= bezier_points[,2]
  Z= bezier_points[,3]
  plot(X1, Y)
  #-----------
  # reduccion
  #-----------
  contx = length(X1) + 1
  conty = 1
  ayuda = 1-0.01
  ayuda2 = 0.02
  ayuda3 = 0.02
  
  repeat
  {
    if(X1[conty]-ayuda3 >= 0)
    {
      X1[contx] = X1[conty] -ayuda3
    }
    else
    {
      #X1[contx] = 0
    }
    
    if(Y[conty] -ayuda2 >= 0)
    {
      Y[contx] = Y[conty] -ayuda2
    }
    else
    {
      Y[contx] = 0
      X1[contx]=X1[contx-1]
    }
    
    Z[contx] = ayuda
    contx = contx + 1
    conty = conty + 1
    
    if(conty == 101) 
    {
      conty = 1
      ayuda = ayuda - 0.01
      ayuda2 = ayuda2 + 0.02
      ayuda3 = ayuda3 + 0.02
      cat("hola", ayuda2, "\n")
    }
    if(contx == 15501)
    {
      cat("z", Z[contx-1], "\n")
    
      break;
    }
  }
  AX= X1[9901:10001]
  AY= Y[9901:10001]
  plot(AX,AY)
  X1n=-1*X1
  Yn=-1*Y
  
  ultimoZ = Z[contx-1]
  
  ultimosx = X1[(contx-101):(contx-1)]
  ultimosy = Y[(contx-101):(contx-1)]
  ultimosz=c()
  
  plot(ultimosx, ultimosy)
  
  contx = 1
  conty = 1
  ayuda = 1-0.01
  ayuda2 = 0.02
  ayuda3 = 0.02
  
  repeat
  {
    ultimosx[contx] = ultimosx[conty] -ayuda3
    ultimosy[contx] = ultimosy[conty] -ayuda2
    ultimosz[contx] = ultimoZ
    contx = contx + 1
    conty = conty + 1
    
    if(conty == 101)
    {
      conty = 1
      ayuda = ayuda - 0.01
      ayuda2 = ayuda2 + 0.02
      ayuda3 = ayuda3 + 0.02
      cat("hola", ayuda2, "\n")
    }
    if(contx == 10001)
    {
      cat("z", Z[contx-1], "\n")
      
      break;
    }
  }
  
  #-----------
  # Gráfico 3D
  #-----------

  library(rgl)
  plot3d(X1, Y, Z, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-1,1))
  
  plot3d(X1n, Y, Z, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-1,1))
  
  plot3d(X1n, Yn, Z, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-1,1))
  
  plot3d(X1, Yn, Z, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-1,1))
  
  #-------------------------
  # Gráfico 3D pared interna
  #-------------------------
  
  z = function(x,y)
  {
    -sqrt(81-x^2-y^2)+8.5
  }
  
  plot3d(z, xlim = c(-7,7), ylim = c(-7,7), zlim =c(-1,1), col = "blue")
  
  #-------------------------
  #  Gráfico piso Mortero
  #-------------------------
  
  uXn=-1*ultimosx
  uYn=-1*ultimosy
  
  plot3d(ultimosx, ultimosy, ultimosz, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-1,1))
  
  plot3d(uXn, ultimosy, ultimosz, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-1,1))
  
  plot3d(uXn, uYn, ultimosz, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-1,1))
  
  plot3d(ultimosx, uYn, ultimosz, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-1,1))
  