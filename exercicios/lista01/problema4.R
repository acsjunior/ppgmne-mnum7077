library(lpSolve)

# Min Z = 2x1 + 3x2 + 5x3
Z <- c(2,3,5)

# S.a:
# x1 + 2x2 + 3x3 >= 7
# 3x1 + 2x2 + 3x3 >= 11
# x1, x3 >= 0 e inteiras
# x2 >= 0

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

# Solução 1
s1 <- lp(direction = "min",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs)

s1
s1$solution
