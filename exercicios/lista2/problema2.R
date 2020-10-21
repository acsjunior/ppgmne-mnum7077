library(lpSolve)

Z <- c(7,9,1,6)

r <- c(8,2,4,2,
       4,8,2,0,
       7,0,6,2)

const_matrix <- matrix(r, ncol = length(Z), byrow = T)
const_direct <- c("<=", "<=", "<=")
const_rhs <- c(16, 20, 11)

si <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs,
         int.vec = c(1,2,4))


si
si$solution
# 48.16667 
# x1 = 0
# x2 = 2
# x3 = 0.1666667
# x4 = 5


# ------------------------------------------------------------------------------
# Camada 0

s0 <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs)

s0
s0$solution
# Z = 55.5
# x1 = 0
# x2 = 2.5
# x3 = 0
# x4 = 5.5

# ------------------------------------------------------------------------------

# Solução 0 com variáveis de folga

Z = c(7,9,1,6,0,0,0,0)

r = c(8,2,4,2,1,0,0,0,
      4,8,2,0,0,1,0,0,
      7,0,6,2,0,0,1,0,
      0.5, 0, 0.25, 0, 0, 0.125, 0, -1)

const_matrix <- matrix(r, ncol = length(Z), byrow = T)
const_direct <- c("=", "=", "=", "=")
const_rhs <- c(16, 20, 11, 0.5)

s0 <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs)

s0
s0$solution







