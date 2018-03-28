### SOLVING EQUATIONS  ###

## finding solution for: Bxy + Dx + Ey + F = 0
# https://www.alpertron.com.ar/METHODS.HTM
# Bxy + Dx + Ey + F = 0
# x = (di - E)/B   ; y = ((DE - BF)/di - D)/B
# where di -> divisors of (DE - BF)

# take inputs:
# 12252xy + 3468x + 8784y - 1833.3 = 0 
B <- 12252 
D <- 3468  
E <- 8784  
F <- as.numeric(-1833.3)  

## save divisor as list
# http://www.dummies.com/programming/r/how-to-round-off-numbers-in-r/
N <- round(D*E - B*F)
# how to find the divisors of a number in R
# https://stackoverflow.com/questions/19465720/writing-a-function-to-calculate-divisors-in-r
# https://stackoverflow.com/questions/6424856/r-function-for-returning-all-factors/6425597#6425597
div <- seq_len(as.integer(N))
divisor <- div[as.integer(N) %% div == 0L]

## calculate values of x & y
for (i in 1:length(divisor) ) {
    x = (divisor[i] - E)/B
    y = (as.integer(N)/divisor[i] - D)/B
    if(x >0 & y>0) {print(paste(x,' ',y))}
    
    x = (-divisor[i] - E)/B
    y = (-as.integer(N)/divisor[i] - D)/B
    if(x >0 & y>0) {print(paste(x,' ',y))}
  }
  
}


###################################################################################################
## solve simultaneous linear equations of the form : ax + by = c

## method 1:
# google: solve linear equations in R
# https://stackoverflow.com/questions/8145694/solving-simultaneous-equations-with-r

a <- rbind(c(3144, 0, 1), 
          c(0, 8348, 1),
          c(3468, 8784, 1))
b <- c(1940.6, 1866.5, 1833.3)

solve(a, b)

##http://friendly.github.io/matlib/articles/linear-equations.html
#library(matlib)
#showEqn(a, b)
#plotEqn3d(a,b)  #plotEqn(a,b)


#####################################################################################################
## solving inequality
# linear programming in R
# google: how to solve inequalities in R;  solve linear inequalities in R
#https://stackoverflow.com/questions/23846217/solving-a-mixed-system-of-equality-and-inequality
#https://stackoverflow.com/questions/24432059/solution-of-system-of-inequalities-in-r

#library(limSolve)
#a <- matrix(nrow = 2, ncol = 2, byrow = TRUE,
#            data = c(-2448, -8252, -2616, -8360))
#b <- c(-1833.3, -2094.5)
#lsei(a,b, verbose = FALSE)
#$X
#[1]  1.7446611 -0.2953987


#library(matlib)
#showEqn(a, b)
#plotEqn3d(a,b)  #plotEqn(a,b)

