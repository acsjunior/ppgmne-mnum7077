library(lpSolve)

Z <- c(2,3,5)

r <- c(1,2,3,
       3,2,3)

const_matrix <- matrix(r, ncol = length(Z), byrow = T)
const_direct <- c(">=", ">=")
const_rhs <- c(7, 11)

si <- lp(direction = "min",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs,
         int.vec = c(1,3))


si
si$solution
# Z = 11.5
# x1 = 2
# x2 = 2.5
# x3 = 0

# ------------------------------------------------------------------------------
# Camada 0

s0 <- lp(direction = "min",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs)

s0
s0$solution
